module special;

import std.stdio;
import std.traits;
import std.algorithm;

template count(T...)
{
    enum count = T.length;
}

// http://forum.dlang.org/post/mailman.3233.1302128675.4748.digitalmars-d-learn@puremagic.com
template partialApply(alias fun, args...)
{
    static if (args.length > (ParameterTypeTuple!fun).length)
    {
        static assert(0, Format!("Tried to pass %s arguments, max is %s.",
                                 count!args, (ParameterTypeTuple!fun).length));
    }

    ReturnType!fun partialApply(T...)(T t)
    {
        return fun(args, t);
    }
}

unittest
{
    ulong foo(string x, ulong y, ulong z)
    {
        return x.length + y + z;
    }

    alias onePar = partialApply!(foo, "bar");
    alias twoPar = partialApply!(foo, "bar", 1);
    alias threePar = partialApply!(foo, "bar", 1, 2);

    assert(onePar(1, 2) == 6);
    assert(twoPar(2) == 6);
    assert(threePar() == 6);
}

int[TCounted] countBy(T, TCounted)(T[] elements, TCounted function(T) counted)
{
    int[TCounted] m;
    foreach (T elem; elements)
    {
        auto key = counted(elem);
        auto count = key in m;
        if (count is null)
            m[key] = 1;
        else
            *count = *count + 1;
    }

    return m;
}

unittest
{
    int[] r = [1, 2, 2, 3, 3, 4, 2, 1, 9];
    auto counts = r.countBy((int x) => x);

    assert(counts[1] == 2);
    assert(counts[2] == 3);
    assert(counts[3] == 2);
    assert(counts[4] == 1);
    assert(counts[9] == 1);
    assert(counts.keys.length == 5);
}

T product(T)(T[] ts)
{
    if (ts.length == 0)
        return 0;
    auto total = ts[0];
    auto tail = ts[1..ts.length];
    foreach (t; tail)
    {
        total *= t;
    }

    return total;
}

T maximum(T)(T[] ts)
{
    return ts.fold!((a, c) => a > c ? a : c);
}