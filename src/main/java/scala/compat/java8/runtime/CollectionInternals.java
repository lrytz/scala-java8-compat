/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.compat.java8.runtime;

// No imports! All type names are fully qualified to avoid confusion!

public class CollectionInternals {
    public static <A> Object[] getTable(scala.collection.mutable.LinkedHashSet<A> hm) { return hm.getTable().table(); }

    public static <A, B> Object[] getTable(scala.collection.mutable.LinkedHashMap<A, B> hm) { return hm.getTable().table(); }

    public static <K> K hashEntryKey(Object hashEntry) { return ((scala.collection.mutable.HashEntry<K, ?>)hashEntry).key(); }
    public static Object hashEntryNext(Object hashEntry) { return ((scala.collection.mutable.HashEntry<?, ?>)hashEntry).next(); }
    public static <V> V linkedEntryValue(Object hashEntry) { return ((scala.collection.mutable.LinkedHashMap.LinkedEntry<?, V>)hashEntry).value(); }

    public static <A> boolean getDirt(scala.collection.immutable.Vector<A> v) { return v.dirty(); }
    public static <A> Object[] getDisplay0(scala.collection.immutable.Vector<A> v) { return v.display0(); }
    public static <A> Object[] getDisplay0(scala.collection.immutable.VectorIterator<A> p) { return p.display0(); }
    public static <A> Object[] getDisplay1(scala.collection.immutable.Vector<A> v) { return v.display1(); }
    public static <A> Object[] getDisplay1(scala.collection.immutable.VectorIterator<A> p) { return p.display1(); }
    public static <A> Object[] getDisplay2(scala.collection.immutable.Vector<A> v) { return v.display2(); }
    public static <A> Object[] getDisplay2(scala.collection.immutable.VectorIterator<A> p) { return p.display2(); }
    public static <A> Object[] getDisplay3(scala.collection.immutable.Vector<A> v) { return v.display3(); }
    public static <A> Object[] getDisplay3(scala.collection.immutable.VectorIterator<A> p) { return p.display3(); }
    public static <A> Object[] getDisplay4(scala.collection.immutable.Vector<A> v) { return v.display4(); }
    public static <A> Object[] getDisplay4(scala.collection.immutable.VectorIterator<A> p) { return p.display4(); }
    public static <A> Object[] getDisplay5(scala.collection.immutable.Vector<A> v) { return v.display5(); }
    public static <A> Object[] getDisplay5(scala.collection.immutable.VectorIterator<A> p) { return p.display5(); }
    public static <A> scala.Tuple2< scala.Tuple2< scala.collection.Iterator<A>, Object >, scala.collection.Iterator<A> > trieIteratorSplit(scala.collection.Iterator<A> it) {
        return null;
    }

    public static long[] getBitSetInternals(scala.collection.mutable.BitSet bitSet) { return bitSet.elems(); }
}

