{- |
Module      :  Language.Javascript.JMacro.Prelude
Copyright   :  (c) Gershom Bazerman, Jeff Polakow 2010
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental
-}

{-# LANGUAGE QuasiQuotes #-}

module Language.Javascript.JMacro.Prelude where
import Language.Javascript.JMacro.Base
import Language.Javascript.JMacro.QQ


-- | This provides a set of basic functional programming primitives, a few utility functions
-- and, more importantly, a decent sample of idiomatic jmacro code. View the source for details.
jmPrelude :: JStat
jmPrelude = [jmacro|

    fun withHourglass f {
          document.body.style.cursor="wait";
          setTimeout(\ {
             try {f();} catch(e) {console.log(e);}
             document.body.style.cursor="default";},
             400);
        };

    fun confirmIt n f {
              var r = confirm("Are you sure you want to " + n + "?");
              if(r) {
                f();
              }
              return false;
            };


    fun memo f {
            var tbl = {};
            return \x {
              var v0 = tbl[x];
              if( v0 == null )
                  { var v1 = f x;
                    tbl[x] = v1;
                    return v1
                  }
                  else { return v0 }
            }
        };

    fun mySplit str xs -> map $.trim (xs.split str);

    fun unquote open close x {
            var special = ["[","]","(",")"];
            fun escape c -> (elem c special ? "\\" : "") + c;
            var rgx = new RegExp("^"+escape open+"(.*)"+escape close+"$"),
                res = x.match(rgx);
            if( res != null ) {return res[1]} else {return x}
        };


    fun head xs -> xs[0];
    fun tail xs -> xs.slice(1);
    fun init xs {xs.pop(); return xs;};
    fun last xs -> xs[xs.length - 1];
    fun cons x xs {
        xs.unshift(x);
        return xs;
    };

    fun id x -> x;
    fun konst x -> \_ -> x;

    fun isEmpty x -> x.length == 0;

    fun notNull x -> x != null;
    fun nullDef def x -> x == null ? def : x;
    fun bindNl x f -> x == null ? null : f x;
    fun tryNull x notNull isNull -> x != null ? notNull x : isNull();

    // -- objFoldl is really just objFold (maps don't have a left and right)

    // --objFoldlEnumLbl :: (label -> b -> a -> (b,Bool)) -> b -> [a] -> b
    // --Provides shortcut escape.
    fun objFoldlEnumLbl f v xs {
        var acc = v;
        for( var i in xs ) {
          if (xs[i] != null) {
            var res = f i acc xs[i];
            acc = res[0];
            if( !res[1] ) { break; }
          };
        };
        return acc;
    };

    // --objFoldlEnum :: (b -> a -> (b,Bool)) -> b -> [a] -> b
    fun objFoldlEnum f v xs -> objFoldlEnumLbl (\_ acc x -> f acc x) v xs;

    // --As above, no shortcut return
    fun objFoldlLbl f v xs {
        var acc = v;
        for( var i in xs) {if (xs[i] != null) {acc = f i acc xs[i]};};
        return acc;
    };
    fun objFoldl f v xs -> objFoldlLbl (\_ acc x -> f acc x) v xs;

    fun mapObjVals f xs -> objFoldlLbl (\lbl acc x {acc[lbl] = f x; return acc}) {} xs

    fun objLength xs -> objFoldl (\n _ -> n + 1) 0 xs;
    fun objIter f xs -> objFoldl (\_ x {f x; return null}) null xs;
    fun objIterLbl f xs -> objFoldlLbl (\l _ v {f l v; return null}) null xs;

    fun objUnion xs ys {
        var res = {};
        for (var i in xs) {res[i] = xs[i]};
        for (var i in ys) {res[i] = ys[i]};
        return res;
    };

    // -- as above, but over arrays.
    fun foldlEnumIdx f v xs {
        var acc = v;
        for( var i = 0; i < xs.length; i = i + 1) {
            var res = f i acc xs[i];
            acc = res[0];
            if( !res[1] ) { break; }
        };
        return acc;
    };

    fun foldlEnum f v xs -> foldlEnumIdx (\_ acc x -> f acc x) v xs;

    fun foldl f v xs {  // -- foldlEnum (\acc x -> [f acc x, true]) v xs;
        var acc = v;
        for( var i = 0; i < xs.length; i++) {acc = f acc xs[i];};
        return acc;
    };

    fun foldl2 f v xs ys {
        var acc = v;
        for( var i = 0; i < xs.length && i < ys.length; i++) {acc = f acc xs[i] ys[i];};
        return acc;
    };

    fun foldr f v xs {
        var res = v;
        for( var i = xs.length - 1; i >= 0; i = i - 1) {res = f xs[i] res;};
        return res;
    };

    // -- IE doesn't treat strings as arrays
    fun strFoldr f v xs {
             var res = v;
             for (var i = xs.length - 1; i >= 0; i = i - 1) {res = f xs.charAt(i) res};
             return res;
    };

    fun max x y -> x > y ? x : y;
    fun min x y -> x < y ? x : y;
    fun maximumOrNull xs -> (xs.length == 0) ? null : foldl max (head xs) (tail xs);
    fun minimumOrNull xs -> (xs.length == 0) ? null : foldl min (head xs) (tail xs);
    fun sum x -> foldl (\a b -> a + b) 0 x;

    // -- ['a','b','c'] --> [['a',0], ['b',1], ['c',2]]
    fun addIndex xs {
            var res = [];
            for( var i = 0; i < xs.length; i++) { res.push([xs[i],i]) };
            return res;
    };


    // -- cmp x y is true when x > y
    fun minimumBy cmp xs -> foldl (\x y -> cmp x y ? x : y) (xs[0]) xs.slice(1);


    fun zipWith f xs ys {
            var res = [],
            l = min xs.length ys.length;
            for(var i = 0; i < l; i++) {
                res.push(f xs[i] ys[i]);
            }
            return res;
    };

    fun zip xs ys -> zipWith (\x y -> [x, y]) xs ys;

    fun zip3 xs ys zs {
            var res = [],
                l = min (min xs.length ys.length) zs.length;
            for(var i = 0; i < l; i++) {
                res.push([xs[i], ys[i], zs[i]]);
            }
            return res;
    };

    fun getTblHash tbl {
            var cols = $("th", $(tbl)).map(\_ x -> $(x).text());
            return map (\r -> foldl2 (\acc c v {acc[c] = $(v).text(); return acc}) {} cols $("td",$(r))) $("tbody tr", $(tbl))
        };

    // -- equality test which ignores case for strings
    fun eq x y {
            var x1 = typeof(x) == "string" ? x.toLowerCase() : x,
                y1 = typeof(y) == "string" ? y.toLowerCase() : y;
            return x1 == y1;
        };

    // -- structural equality
    fun equals x y {
       if(x===y) {return true;}
       if(typeof x != typeof y) {return false;}
       if($.isArray x && $.isArray y) {
          for(var n in x) {
             if (!(equals x[n] y[n])) {return false;}
          }
          return true;
       }
       return x == y;
    }

    fun map f xs -> foldl (\acc x {acc.push(f x); return acc}) [] xs;
    fun filter p xs -> foldl (\acc x {if (p x) {acc.push(x)}; return acc}) [] xs
    fun mapFilter p f xs -> foldl (\acc x {if (p x) {acc.push(f x)}; return acc}) [] xs
    fun concat xs -> foldl (\acc x -> acc.concat(x) ) [] xs

    fun toList xs -> map id xs;  // -- this can turn a jQuery object into a real list

    fun all p xs -> foldlEnum (\_ x -> [p x, p x]) true xs;

    fun findWithIdx p xs -> foldlEnumIdx (\i failure x -> p i x ? [x, false] : [failure, true]) null xs;
    fun findIdx p xs -> foldlEnumIdx (\i failure x -> p x ? [i, false] : [failure, true]) null xs;

    fun find p xs -> findWithIdx (\_ x -> p x) xs;

    fun elem x xs -> tryNull (find (\y -> x == y) xs) (konst true) (konst false);

    fun isPrefixOf x xs -> xs.search(new RegExp("^"+x)) != -1;

    // -- sortOn :: Ord b => (a -> b) -> [a] -> [a]
    fun sortOn f arr {
         fun cmpFun x y {
               var xv = f x,
                   yv = f y;
               if (xv == yv) {return 0};
               if (xv == null) {return 1};
               if (yv == null) {return -1};
               return xv > yv ? 1 : -1
         };
         arr.sort(cmpFun);
         return arr;
    }

    fun hashOn f xs {
      var hash = {};
      fun pushAttr x {
         var atr = f x;
         if( atr != null ) {
           if( hash[atr] == null ) { hash[atr] = [] };
           hash[atr].push(x)
         };
      };
      map pushAttr xs;
      return hash;
    }

    fun groupOn f xs {
       var hash = hashOn f xs;
       return objFoldl (\acc x {acc.push x; return acc;}) [] hash;
    }

    fun chunkBy x ys -> x >= ys.length ? [ys]
                                       : cons (ys.slice(0,x)) (chunkBy x (ys.slice(x)));

    fun transpose xxs {
      if (xxs.length == 0) {return []};
      if (xxs[0].length == 0) {return transpose (tail xxs);};
      var x = xxs[0][0],
          xs = xxs[0].slice(1),
          xss = xxs.slice(1);
      return cons (cons x (map head xss)) (transpose (cons xs (map tail xss))) // -- (x : map head xss) : transpose (xs : map tail xss)
    }


    // -- idxs is an array of (index, sort ascending?) pairs
    fun multiIdxSortGen idxs cmpFun modFun xs {
        var f = \x y -> \i acc idxsi {
                var cmp = cmpFun (modFun x)[i] (modFun y)[i];
                return (cmp != 0) ? [idxsi[1] ? cmp : cmp * (-1), false] : [acc, true]
            };

        xs.sort( \x y -> foldlEnumIdx (f x y) 0 idxs );
    };

    // --A few statistical funcutions.

    // -- ordinary least squares
    fun ols xs ys {
        if (xs.length != ys.length) {return null};
        var n = xs.length,
            sx = sum xs,
            sx2 = sum (map (\x -> x*x) xs),
            sy = sum ys,
            sxy = sum (zipWith (\x y -> x * y) xs ys),
            bot = n * sx2 - sx * sx,
            m = (n * sxy - sy*sx) / bot,
            b = (sy * sx2 - sx * sxy) / bot;
        return [m,b];
    };

    // -- Linear regression
    fun doRegress xs {
              var xvs = map (\x -> x[0]) xs,
                  yvs = map (\x -> x[1]) xs,
                  regressres = ols xvs yvs,
                  m = regressres[0],
                  b = regressres[1],
                  yvs1 = map (\x -> m*x+b) xvs,
                  ymean = mean yvs,
                  sstot = sum (map (\y -> Math.pow (y - ymean) 2) yvs),
                  sserr = sum (zipWith (\y f -> Math.pow (y - f) 2) yvs yvs1),
                  xsNew = zipWith (\x y -> [x,y,x,y.toPrecision(2),""]) xvs yvs1;
              return [xsNew, (1 - (sserr/sstot)).toPrecision(4)];
    };


    fun mean xs {
       var res = xs[0];
       for (var i = 1;  i < xs.length; i++) {
           var x = xs[i];
           var delta = x - res;
           var sweep = i + 1.0;
           res = res + (delta / sweep);
           // -- sqsum += delta * delta * (i / sweep);
       }
       return res;
    };

    fun stdev xs {  // -- Knuth's standard deviation algorithm, returns [stdDev, mean, size]
        var n = 0,
            mean = 0,
            s = 0;
        for (var i = 0; i < xs.length; i++) {
            n = n + 1;
            var delta = xs[i] - mean;
            mean = mean + delta/n;
            s = s + delta*(xs[i] - mean);  // -- this expression uses the new value of mean
        };
        return [Math.sqrt (s/(n - 1)), mean, n];
    };

|]
