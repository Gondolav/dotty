
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcIJJ$sp extends JFunction2 {
    abstract int apply$mcIJJ$sp(long v1, long v2);

    default Object apply(Object v1, Object v2) { return (Integer) apply$mcIJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); }
}
