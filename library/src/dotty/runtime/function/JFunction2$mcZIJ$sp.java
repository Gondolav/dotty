
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcZIJ$sp extends JFunction2 {
    abstract boolean apply$mcZIJ$sp(int v1, long v2);

    default Object apply(Object v1, Object v2) { return (Boolean) apply$mcZIJ$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); }
}
