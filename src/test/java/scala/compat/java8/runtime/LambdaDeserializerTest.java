package scala.compat.java8.runtime;

import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.Arrays;

public final class LambdaDeserializerTest {
    private LambdaHost lambdaHost = new LambdaHost();

    @Test
    public void serializationPrivate() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByPrivateImplMethod();
        F1<Boolean, String> f2 = reconstitute(f1);
        Assert.assertEquals(f1.apply(true), f2.apply(true));
    }

    @Test
    public void serializationStatic() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        F1<Boolean, String> f2 = reconstitute(f1);
        Assert.assertEquals(f1.apply(true), f2.apply(true));
    }

    @Test
    public void implMethodNameChanged() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        SerializedLambda sl = writeReplace(f1);
        checkIllegalAccess(copySerializedLambda(sl, sl.getImplMethodName() + "___", sl.getImplMethodSignature()));
    }

    @Test
    public void implMethodSignatureChanged() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        SerializedLambda sl = writeReplace(f1);
        checkIllegalAccess(copySerializedLambda(sl, sl.getImplMethodName(), sl.getImplMethodSignature().replace("Boolean", "Integer")));
    }

    private void checkIllegalAccess(SerializedLambda serialized) {
        try {
            LambdaDeserializer.deserializeLambda(MethodHandles.lookup(), serialized);
            throw new AssertionError();
        } catch (IllegalArgumentException iae) {
            if (!iae.getMessage().contains("Illegal lambda deserialization")) {
                Assert.fail("Unexpected message: " + iae.getMessage());
            }
        }
    }

    private SerializedLambda copySerializedLambda(SerializedLambda sl, String implMethodName, String implMethodSignature) {
        Object[] captures = new Object[sl.getCapturedArgCount()];
        for (int i = 0; i < captures.length; i++) {
            captures[i] = sl.getCapturedArg(i);
        }
        return new SerializedLambda(loadClass(sl.getCapturingClass()), sl.getFunctionalInterfaceClass(), sl.getFunctionalInterfaceMethodName(),
                sl.getFunctionalInterfaceMethodSignature(), sl.getImplMethodKind(), sl.getImplClass(), implMethodName, implMethodSignature,
                sl.getInstantiatedMethodType(), captures);
    }

    private Class<?> loadClass(String className) {
        try {
            return Class.forName(className.replace('/', '.'));
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private <A, B> F1<A, B> reconstitute(F1<A, B> f1) {
        try {
            return (F1<A, B>) LambdaDeserializer.deserializeLambda(LambdaHost.lookup(), writeReplace(f1));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private <A, B> SerializedLambda writeReplace(F1<A, B> f1) {
        try {
            Method writeReplace = f1.getClass().getDeclaredMethod("writeReplace");
            writeReplace.setAccessible(true);
            return (SerializedLambda) writeReplace.invoke(f1);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}


interface F1<A, B> extends Serializable {
    B apply(A a);
}

class LambdaHost {
    public F1<Boolean, String> lambdaBackedByPrivateImplMethod() {
        int local = 42;
        return (b) -> Arrays.asList(local, b ? "true" : "false", LambdaHost.this).toString();
    }

    public F1<Boolean, String> lambdaBackedByStaticImplMethod() {
        int local = 42;
        return (b) -> Arrays.asList(local, b ? "true" : "false", LambdaHost.this).toString();
    }

    public static MethodHandles.Lookup lookup() { return MethodHandles.lookup(); }
}
