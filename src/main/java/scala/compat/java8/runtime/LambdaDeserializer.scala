package scala.compat.java8.runtime

import java.lang.invoke._
import java.lang.ref.WeakReference

/**
 * This class is only intended to be called by synthetic `$deserializeLambda$` method that the Scala 2.12
 * compiler will add to classes hosting lambdas.
 *
 * It is intended to be consumed directly.
 */
object LambdaDeserializer {
  private final case class CacheKey(implClass: Class[_], implMethodName: String, implMethodSignature: String)
  private val cache = new java.util.WeakHashMap[CacheKey, WeakReference[CallSite]]()

  /**
   * Deserialize a lambda by calling `LambdaMetafactory.altMetafactory` to spin up a lambda class
   * and instantiating this class with the captured arguments.
   *
   * A cache is employed to ensure that subsequent deserialization of the same lambda expression
   * is cheap, it amounts to a reflective call to the constructor of the previously created class.
   * However, deserialization of the same lambda expression is not guaranteed to use the same class,
   * concurrent deserialization of the same lambda expression may spin up more than one class.
   *
   * This cache is weak in keys and values to avoid retention of the enclosing class (and its classloader)
   * of deserialized lambdas.
   *
   * Assumptions:
   *  - No additional marker interfaces are required beyond `{java.io,scala.}Serializable`. These are
   *    not stored in `SerializedLambda`, so we can't reconstitute them.
   *  - No additional bridge methods are passed to `altMetafactory`. Again, these are not stored.
   *
   * Note: The Java compiler
   *
   * @param lookup      The factory for method handles. Must have access to the implementation method, the
   *                    functional interface class, and `java.io.Serializable` or `scala.Serializable` as
   *                    required.
   * @param serialized  The lambda to deserialize. Note that this is typically created by the `readResolve`
   *                    member of the anonymous class created by `LambdaMetaFactory`.
   * @return            An instance of the functional interface
   */
  def deserializeLambda(lookup: MethodHandles.Lookup, serialized: SerializedLambda): AnyRef = {
    def slashDot(name: String) = name.replaceAll("/", ".")
    val loader = lookup.lookupClass().getClassLoader
    val implClass = loader.loadClass(slashDot(serialized.getImplClass))

    def makeCallSite: CallSite = {
      import serialized._
      def parseDescriptor(s: String) =
        MethodType.fromMethodDescriptorString(s, loader)

      val funcInterfacesSignature = parseDescriptor(getFunctionalInterfaceMethodSignature)
      val methodType: MethodType = funcInterfacesSignature
      val instantiated = parseDescriptor(getInstantiatedMethodType)
      val implMethodSig = parseDescriptor(getImplMethodSignature)

      val from = implMethodSig.parameterCount() - funcInterfacesSignature.parameterCount()
      val to = implMethodSig.parameterCount()
      val functionalInterfaceClass = loader.loadClass(slashDot(getFunctionalInterfaceClass))
      var invokedType: MethodType =
        implMethodSig.dropParameterTypes(from, to)
          .changeReturnType(functionalInterfaceClass)

      val implMethod: MethodHandle = try {
        getImplMethodKind match {
          case MethodHandleInfo.REF_invokeStatic =>
            lookup.findStatic(implClass, getImplMethodName, implMethodSig)
          case MethodHandleInfo.REF_invokeVirtual =>
            invokedType = invokedType.insertParameterTypes(0, implClass)
            lookup.findVirtual(implClass, getImplMethodName, implMethodSig)
          case MethodHandleInfo.REF_invokeSpecial =>
            invokedType = invokedType.insertParameterTypes(0, implClass)
            lookup.findSpecial(implClass, getImplMethodName, implMethodSig, implClass)
        }
      } catch {
        case e: ReflectiveOperationException =>
          throw new IllegalArgumentException("Illegal lambda deserialization", e)
      }
      val FLAG_SERIALIZABLE = 1
      val FLAG_MARKERS = 2
      val flags: Int = FLAG_SERIALIZABLE | FLAG_MARKERS
      val markerInterface: Class[_] = if (functionalInterfaceClass.getName.startsWith("scala.Function"))
        loader.loadClass("scala.Serializable")
      else
        loader.loadClass("java.io.Serializable")

      LambdaMetafactory.altMetafactory(
        lookup, getFunctionalInterfaceMethodName, invokedType,

        /* samMethodType          = */ funcInterfacesSignature,
        /* implMethod             = */ implMethod,
        /* instantiatedMethodType = */ instantiated,
        /* flags                  = */ flags.asInstanceOf[AnyRef],
        /* markerInterfaceCount   = */ 1.asInstanceOf[AnyRef],
        /* markerInterfaces[0]    = */ markerInterface,
        /* bridgeCount            = */ 0.asInstanceOf[AnyRef]
      )
    }

    val key = new CacheKey(implClass, serialized.getImplMethodName, serialized.getImplMethodSignature)
    val callSiteRef: WeakReference[CallSite] = cache.get(key)
    var site = if (callSiteRef == null) null else callSiteRef.get()
    if (site == null) {
      site = makeCallSite
      cache.put(key, new WeakReference(site))
    }

    val factory = site.getTarget
    val captures = Array.tabulate(serialized.getCapturedArgCount)(n => serialized.getCapturedArg(n))
    factory.invokeWithArguments(captures: _*)
  }
}
