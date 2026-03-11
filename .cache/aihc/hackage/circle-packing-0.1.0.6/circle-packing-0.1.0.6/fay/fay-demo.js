/** @constructor
*/
var FayDemo = function(){
var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
  while (thunkish instanceof $) {
    thunkish = thunkish.force(nocache);
  }
  return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
  var f = arguments[0];
  for (var i = 1, len = arguments.length; i < len; i++) {
    f = (f instanceof $? _(f) : f)(arguments[i]);
  }
  return f;
}

// Thunk object.
function $(value){
  this.forced = false;
  this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

function Fay$$seq(x) {
  return function(y) {
    _(x,false);
    return y;
  }
}

function Fay$$seq$36$uncurried(x,y) {
  _(x,false);
  return y;
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
  this.value = value;
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then(a){
  return function(b){
    return Fay$$bind(a)(function(_){
      return b;
    });
  };
}

// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
// >>
function Fay$$then$36$uncurried(a,b){
  return Fay$$bind$36$uncurried(a,function(_){ return b; });
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind(m){
  return function(f){
    return new $(function(){
      var monad = _(m,true);
      return f(monad.value);
    });
  };
}

// >>=
// This is used directly from Fay, but can be rebound or shadowed. See primOps in Types.hs.
function Fay$$bind$36$uncurried(m,f){
    return new $(function(){
      var monad = _(m,true);
      return f(monad.value);
    });
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$$_return(a){
  return new Fay$$Monad(a);
}

// Allow the programmer to access thunk forcing directly.
function Fay$$force(thunk){
  return function(type){
    return new $(function(){
      _(thunk,type);
      return new Fay$$Monad(Fay$$unit);
    })
  }
}

// This is used directly from Fay, but can be rebound or shadowed.
function Fay$$return$36$uncurried(a){
  return new Fay$$Monad(a);
}

// Unit: ().
var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
  var base = type[0];
  var args = type[1];
  var jsObj;
  switch(base){
    case "action": {
      // A nullary monadic action. Should become a nullary JS function.
      // Fay () -> function(){ return ... }
      jsObj = function(){
        return Fay$$fayToJs(args[0],_(fayObj,true).value);
      };
      break;
    }
    case "function": {
      // A proper function.
      jsObj = function(){
        var fayFunc = fayObj;
        var return_type = args[args.length-1];
        var len = args.length;
        // If some arguments.
        if (len > 1) {
          // Apply to all the arguments.
          fayFunc = _(fayFunc,true);
          // TODO: Perhaps we should throw an error when JS
          // passes more arguments than Haskell accepts.
          for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
            // Unserialize the JS values to Fay for the Fay callback.
            fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
          }
          // Finally, serialize the Fay return value back to JS.
          var return_base = return_type[0];
          var return_args = return_type[1];
          // If it's a monadic return value, get the value instead.
          if(return_base == "action") {
            return Fay$$fayToJs(return_args[0],fayFunc.value);
          }
          // Otherwise just serialize the value direct.
          else {
            return Fay$$fayToJs(return_type,fayFunc);
          }
        } else {
          throw new Error("Nullary function?");
        }
      };
      break;
    }
    case "string": {
      // Serialize Fay string to JavaScript string.
      var str = "";
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        str += fayObj.car;
        fayObj = _(fayObj.cdr);
      }
      jsObj = str;
      break;
    }
    case "list": {
      // Serialize Fay list to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[0],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "tuple": {
      // Serialize Fay tuple to JavaScript array.
      var arr = [];
      fayObj = _(fayObj);
      var i = 0;
      while(fayObj instanceof Fay$$Cons) {
        arr.push(Fay$$fayToJs(args[i++],fayObj.car));
        fayObj = _(fayObj.cdr);
      }
      jsObj = arr;
      break;
    }
    case "defined": {
      fayObj = _(fayObj);
      if (fayObj instanceof $_Language$Fay$Stdlib$Undefined) {
        jsObj = undefined;
      } else {
        jsObj = Fay$$fayToJs(args[0],fayObj["slot1"]);
      }
      break;
    }
    case "double": {
      // Serialize double, just force the argument. Doubles are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "int": {
      // Serialize int, just force the argument. Ints are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "bool": {
      // Bools are unboxed.
      jsObj = _(fayObj);
      break;
    }
    case "unknown":
    case "user": {
      if(fayObj instanceof $)
        fayObj = _(fayObj);
      jsObj = Fay$$fayToJsUserDefined(type,fayObj);
      break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
  var base = type[0];
  var args = type[1];
  var fayObj;
  switch(base){
    case "action": {
      // Unserialize a "monadic" JavaScript return value into a monadic value.
      fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
      break;
    }
    case "string": {
      // Unserialize a JS string into Fay list (String).
      fayObj = Fay$$list(jsObj);
      break;
    }
    case "list": {
      // Unserialize a JS array into a Fay list ([a]).
      var serializedList = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedList);
      break;
    }
    case "tuple": {
      // Unserialize a JS array into a Fay tuple ((a,b,c,...)).
      var serializedTuple = [];
      for (var i = 0, len = jsObj.length; i < len; i++) {
        // Unserialize each JS value into a Fay value, too.
        serializedTuple.push(Fay$$jsToFay(args[i],jsObj[i]));
      }
      // Pop it all in a Fay list.
      fayObj = Fay$$list(serializedTuple);
      break;
    }
    case "defined": {
      if (jsObj === undefined) {
        fayObj = new $_Language$Fay$Stdlib$Undefined();
      } else {
        fayObj = new $_Language$Fay$Stdlib$Defined(Fay$$jsToFay(args[0],jsObj));
      }
      break;
    }
    case "double": {
      // Doubles are unboxed, so there's nothing to do.
      fayObj = jsObj;
      break;
    }
    case "int": {
      // Int are unboxed, so there's no forcing to do.
      // But we can do validation that the int has no decimal places.
      // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
      fayObj = Math.round(jsObj);
      if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!";
      break;
    }
    case "bool": {
      // Bools are unboxed.
      fayObj = jsObj;
      break;
    }
    case "unknown":
    case "user": {
      if (jsObj && jsObj['instance']) {
        fayObj = Fay$$jsToFayUserDefined(type,jsObj);
      }
      else
        fayObj = jsObj;
      break;
    }
  default: throw new Error("Unhandled JS->Fay translation type: " + base);
  }
  return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
  this.car = car;
  this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
  var out = null;
  for(var i=xs.length-1; i>=0;i--)
    out = new Fay$$Cons(xs[i],out);
  return out;
}

// Built-in list cons.
function Fay$$cons(x){
  return function(y){
    return new Fay$$Cons(x,y);
  };
}

// List index.
function Fay$$index(index){
  return function(list){
    for(var i = 0; i < index; i++) {
      list = _(list).cdr;
    }
    return list.car;
  };
}

// List length.
function Fay$$listLen(list,max){
  for(var i = 0; list !== null && i < max + 1; i++) {
    list = _(list).cdr;
  }
  return i == max;
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
  return function(y){
    return new $(function(){
      return _(x) * _(y);
    });
  };
}

function Fay$$mult$36$uncurried(x,y){

    return new $(function(){
      return _(x) * _(y);
    });

}

// Built-in +.
function Fay$$add(x){
  return function(y){
    return new $(function(){
      return _(x) + _(y);
    });
  };
}

// Built-in +.
function Fay$$add$36$uncurried(x,y){

    return new $(function(){
      return _(x) + _(y);
    });

}

// Built-in -.
function Fay$$sub(x){
  return function(y){
    return new $(function(){
      return _(x) - _(y);
    });
  };
}
// Built-in -.
function Fay$$sub$36$uncurried(x,y){

    return new $(function(){
      return _(x) - _(y);
    });

}

// Built-in /.
function Fay$$div(x){
  return function(y){
    return new $(function(){
      return _(x) / _(y);
    });
  };
}

// Built-in /.
function Fay$$div$36$uncurried(x,y){

    return new $(function(){
      return _(x) / _(y);
    });

}

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
  // Simple case
  lit1 = _(lit1);
  lit2 = _(lit2);
  if (lit1 === lit2) {
    return true;
  }
  // General case
  if (lit1 instanceof Array) {
    if (lit1.length != lit2.length) return false;
    for (var len = lit1.length, i = 0; i < len; i++) {
      if (!Fay$$equal(lit1[i], lit2[i])) return false;
    }
    return true;
  } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
    do {
      if (!Fay$$equal(lit1.car,lit2.car))
        return false;
      lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
      if (lit1 === null || lit2 === null)
        return lit1 === lit2;
    } while (true);
  } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
             lit1.constructor === lit2.constructor) {
    for(var x in lit1) {
      if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
           Fay$$equal(lit1[x],lit2[x])))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

// Built-in ==.
function Fay$$eq(x){
  return function(y){
    return new $(function(){
      return Fay$$equal(x,y);
    });
  };
}

function Fay$$eq$36$uncurried(x,y){

    return new $(function(){
      return Fay$$equal(x,y);
    });

}

// Built-in /=.
function Fay$$neq(x){
  return function(y){
    return new $(function(){
      return !(Fay$$equal(x,y));
    });
  };
}

// Built-in /=.
function Fay$$neq$36$uncurried(x,y){

    return new $(function(){
      return !(Fay$$equal(x,y));
    });

}

// Built-in >.
function Fay$$gt(x){
  return function(y){
    return new $(function(){
      return _(x) > _(y);
    });
  };
}

// Built-in >.
function Fay$$gt$36$uncurried(x,y){

    return new $(function(){
      return _(x) > _(y);
    });

}

// Built-in <.
function Fay$$lt(x){
  return function(y){
    return new $(function(){
      return _(x) < _(y);
    });
  };
}


// Built-in <.
function Fay$$lt$36$uncurried(x,y){

    return new $(function(){
      return _(x) < _(y);
    });

}


// Built-in >=.
function Fay$$gte(x){
  return function(y){
    return new $(function(){
      return _(x) >= _(y);
    });
  };
}

// Built-in >=.
function Fay$$gte$36$uncurried(x,y){

    return new $(function(){
      return _(x) >= _(y);
    });

}

// Built-in <=.
function Fay$$lte(x){
  return function(y){
    return new $(function(){
      return _(x) <= _(y);
    });
  };
}

// Built-in <=.
function Fay$$lte$36$uncurried(x,y){

    return new $(function(){
      return _(x) <= _(y);
    });

}

// Built-in &&.
function Fay$$and(x){
  return function(y){
    return new $(function(){
      return _(x) && _(y);
    });
  };
}

// Built-in &&.
function Fay$$and$36$uncurried(x,y){

    return new $(function(){
      return _(x) && _(y);
    });
 ;
}

// Built-in ||.
function Fay$$or(x){
  return function(y){
    return new $(function(){
      return _(x) || _(y);
    });
  };
}

// Built-in ||.
function Fay$$or$36$uncurried(x,y){

    return new $(function(){
      return _(x) || _(y);
    });

}

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
  this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
  ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
  return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
  return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */

var Language$Fay$Stdlib$error = function($p1){return new $(function(){var str = $p1;return (function($tmp1){if (_($tmp1) === 0) {return _(Language$Fay$Stdlib$error)(str);}return _(Language$Fay$Stdlib$error)(str);})(_(Language$Fay$Stdlib$error$39$)(str));});};var Language$Fay$Stdlib$error$39$ = function($p1){return new $(function(){return Fay$$jsToFay(["int"],(function() { throw Fay$$fayToJs(["string"],$p1) })());});};var Language$Fay$Stdlib$$_undefined = new $(function(){return _(Language$Fay$Stdlib$error)(Fay$$list("Prelude.undefined"));});var Language$Fay$Stdlib$show = function($p1){return new $(function(){return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["unknown"],$p1)));});};var $_Language$Fay$Stdlib$Undefined = function(){};var Language$Fay$Stdlib$Undefined = new $(function(){return new $_Language$Fay$Stdlib$Undefined();});var $_Language$Fay$Stdlib$Defined = function(slot1){this.slot1 = slot1;};var Language$Fay$Stdlib$Defined = function(slot1){return new $(function(){return new $_Language$Fay$Stdlib$Defined(slot1);});};var $_Language$Fay$Stdlib$Left = function(slot1){this.slot1 = slot1;};var Language$Fay$Stdlib$Left = function(slot1){return new $(function(){return new $_Language$Fay$Stdlib$Left(slot1);});};var $_Language$Fay$Stdlib$Right = function(slot1){this.slot1 = slot1;};var Language$Fay$Stdlib$Right = function(slot1){return new $(function(){return new $_Language$Fay$Stdlib$Right(slot1);});};var Language$Fay$Stdlib$either = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) instanceof $_Language$Fay$Stdlib$Left) {var a = _($p3).slot1;var f = $p1;return _(f)(a);}if (_($p3) instanceof $_Language$Fay$Stdlib$Right) {var b = _($p3).slot1;var g = $p2;return _(g)(b);}throw ["unhandled case in either",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$fromInteger = function($p1){return new $(function(){var x = $p1;return x;});};var Language$Fay$Stdlib$fromRational = function($p1){return new $(function(){var x = $p1;return x;});};var Language$Fay$Stdlib$negate = function($p1){return new $(function(){var x = $p1;return (-(_(x)));});};var Language$Fay$Stdlib$abs = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(0)) ? _(Language$Fay$Stdlib$negate)(x) : x;});};var Language$Fay$Stdlib$signum = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$gt)(_(x))(0)) ? 1 : _(_(_(Fay$$eq)(x))(0)) ? 0 : (-(1));});};var Language$Fay$Stdlib$pi = new $(function(){return Fay$$jsToFay(["double"],Math.PI);});var Language$Fay$Stdlib$exp = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.exp(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$sqrt = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.sqrt(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$log = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.log(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$$42$$42$ = new $(function(){return Language$Fay$Stdlib$unsafePow;});var Language$Fay$Stdlib$$94$$94$ = new $(function(){return Language$Fay$Stdlib$unsafePow;});var Language$Fay$Stdlib$unsafePow = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.pow(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Language$Fay$Stdlib$$94$ = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;if (_(_(Fay$$lt)(_(b))(0))) {return _(Language$Fay$Stdlib$error)(Fay$$list("(^): negative exponent"));} else {if (_(_(_(Fay$$eq)(b))(0))) {return 1;} else {if (_(_(Language$Fay$Stdlib$even)(b))) {return (function(){var x = new $(function(){return _(_(Language$Fay$Stdlib$$94$)(a))(_(_(Language$Fay$Stdlib$quot)(b))(2));});return _(Fay$$mult)(_(x))(_(x));})();}}}var b = $p2;var a = $p1;return _(Fay$$mult)(_(a))(_(_(_(Language$Fay$Stdlib$$94$)(a))(_(Fay$$sub)(_(b))(1))));});};};var Language$Fay$Stdlib$logBase = function($p1){return function($p2){return new $(function(){var x = $p2;var b = $p1;return _(Fay$$div)(_(_(Language$Fay$Stdlib$log)(x)))(_(_(Language$Fay$Stdlib$log)(b)));});};};var Language$Fay$Stdlib$sin = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.sin(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$tan = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.tan(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$cos = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.cos(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$asin = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.asin(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$atan = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.atan(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$acos = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Math.acos(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$sinh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Fay$$sub)(_(_(Language$Fay$Stdlib$exp)(x)))(_(_(Language$Fay$Stdlib$exp)((-(_(x))))))))(2);});};var Language$Fay$Stdlib$tanh = function($p1){return new $(function(){var x = $p1;return (function(){var a = new $(function(){return _(Language$Fay$Stdlib$exp)(x);});var b = new $(function(){return _(Language$Fay$Stdlib$exp)((-(_(x))));});return _(Fay$$div)(_(_(Fay$$sub)(_(a))(_(b))))(_(_(Fay$$add)(_(a))(_(b))));})();});};var Language$Fay$Stdlib$cosh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Fay$$add)(_(_(Language$Fay$Stdlib$exp)(x)))(_(_(Language$Fay$Stdlib$exp)((-(_(x))))))))(2);});};var Language$Fay$Stdlib$asinh = function($p1){return new $(function(){var x = $p1;return _(Language$Fay$Stdlib$log)(_(Fay$$add)(_(x))(_(_(Language$Fay$Stdlib$sqrt)(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(x))(2)))(1)))));});};var Language$Fay$Stdlib$atanh = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(_(_(Language$Fay$Stdlib$log)(_(Fay$$div)(_(_(Fay$$add)(1)(_(x))))(_(_(Fay$$sub)(1)(_(x)))))))(2);});};var Language$Fay$Stdlib$acosh = function($p1){return new $(function(){var x = $p1;return _(Language$Fay$Stdlib$log)(_(Fay$$add)(_(x))(_(_(Language$Fay$Stdlib$sqrt)(_(Fay$$sub)(_(_(_(Language$Fay$Stdlib$$42$$42$)(x))(2)))(1)))));});};var Language$Fay$Stdlib$properFraction = function($p1){return new $(function(){var x = $p1;return (function(){var a = new $(function(){return _(Language$Fay$Stdlib$truncate)(x);});return Fay$$list([a,_(Fay$$sub)(_(x))(_(_(Language$Fay$Stdlib$fromIntegral)(a)))]);})();});};var Language$Fay$Stdlib$truncate = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(0)) ? _(Language$Fay$Stdlib$ceiling)(x) : _(Language$Fay$Stdlib$floor)(x);});};var Language$Fay$Stdlib$round = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.round(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$ceiling = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.ceil(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$floor = function($p1){return new $(function(){return Fay$$jsToFay(["int"],Math.floor(Fay$$fayToJs(["double"],$p1)));});};var Language$Fay$Stdlib$subtract = new $(function(){return _(Language$Fay$Stdlib$flip)(Fay$$sub);});var Language$Fay$Stdlib$even = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$eq)(_(_(Language$Fay$Stdlib$rem)(x))(2)))(0);});};var Language$Fay$Stdlib$odd = function($p1){return new $(function(){var x = $p1;return _(Language$Fay$Stdlib$not)(_(Language$Fay$Stdlib$even)(x));});};var Language$Fay$Stdlib$gcd = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {var x = $p1;return x;}var y = $p2;var x = $p1;return _(_(go)(y))(_(_(Language$Fay$Stdlib$rem)(x))(y));});};};return _(_(go)(_(Language$Fay$Stdlib$abs)(a)))(_(Language$Fay$Stdlib$abs)(b));})();});};};var Language$Fay$Stdlib$lcm = function($p1){return function($p2){return new $(function(){if (_($p2) === 0) {return 0;}if (_($p1) === 0) {return 0;}var b = $p2;var a = $p1;return _(Language$Fay$Stdlib$abs)(_(Fay$$mult)(_(_(_(Language$Fay$Stdlib$quot)(a))(_(_(Language$Fay$Stdlib$gcd)(a))(b))))(_(b)));});};};var Language$Fay$Stdlib$curry = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var f = $p1;return _(f)(Fay$$list([x,y]));});};};};var Language$Fay$Stdlib$uncurry = function($p1){return function($p2){return new $(function(){var p = $p2;var f = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var x = Fay$$index(0)(_($tmp1));var y = Fay$$index(1)(_($tmp1));return _(_(f)(x))(y);}return (function(){ throw (["unhandled case",$tmp1]); })();})(p);});};};var Language$Fay$Stdlib$snd = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(1)(_($p1));return x;}throw ["unhandled case in snd",[$p1]];});};var Language$Fay$Stdlib$fst = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(0)(_($p1));return x;}throw ["unhandled case in fst",[$p1]];});};var Language$Fay$Stdlib$find = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(Language$Fay$Stdlib$Just)(x) : _(_(Language$Fay$Stdlib$find)(p))(xs);}if (_($p2) === null) {return Language$Fay$Stdlib$Nothing;}throw ["unhandled case in find",[$p1,$p2]];});};};var Language$Fay$Stdlib$filter = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$filter)(p))(xs)) : _(_(Language$Fay$Stdlib$filter)(p))(xs);}if (_($p2) === null) {return null;}throw ["unhandled case in filter",[$p1,$p2]];});};};var Language$Fay$Stdlib$not = function($p1){return new $(function(){var p = $p1;return _(p) ? false : true;});};var Language$Fay$Stdlib$$_null = function($p1){return new $(function(){if (_($p1) === null) {return true;}return false;});};var Language$Fay$Stdlib$map = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(f)(x)))(_(_(Language$Fay$Stdlib$map)(f))(xs));}throw ["unhandled case in map",[$p1,$p2]];});};};var Language$Fay$Stdlib$nub = function($p1){return new $(function(){var ls = $p1;return _(_(Language$Fay$Stdlib$nub$39$)(ls))(null);});};var Language$Fay$Stdlib$nub$39$ = function($p1){return function($p2){return new $(function(){if (_($p1) === null) {return null;}var ls = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(_(Language$Fay$Stdlib$elem)(x))(ls)) ? _(_(Language$Fay$Stdlib$nub$39$)(xs))(ls) : _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$nub$39$)(xs))(_(_(Fay$$cons)(x))(ls)));}throw ["unhandled case in nub'",[$p1,$p2]];});};};var Language$Fay$Stdlib$elem = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var y = $tmp1.car;var ys = $tmp1.cdr;var x = $p1;return _(Fay$$or)(_(_(_(Fay$$eq)(x))(y)))(_(_(_(Language$Fay$Stdlib$elem)(x))(ys)));}if (_($p2) === null) {return false;}throw ["unhandled case in elem",[$p1,$p2]];});};};var Language$Fay$Stdlib$notElem = function($p1){return function($p2){return new $(function(){var ys = $p2;var x = $p1;return _(Language$Fay$Stdlib$not)(_(_(Language$Fay$Stdlib$elem)(x))(ys));});};};var $_Language$Fay$Stdlib$GT = function(){};var Language$Fay$Stdlib$GT = new $(function(){return new $_Language$Fay$Stdlib$GT();});var $_Language$Fay$Stdlib$LT = function(){};var Language$Fay$Stdlib$LT = new $(function(){return new $_Language$Fay$Stdlib$LT();});var $_Language$Fay$Stdlib$EQ = function(){};var Language$Fay$Stdlib$EQ = new $(function(){return new $_Language$Fay$Stdlib$EQ();});var Language$Fay$Stdlib$sort = new $(function(){return _(Language$Fay$Stdlib$sortBy)(Language$Fay$Stdlib$compare);});var Language$Fay$Stdlib$compare = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Fay$$gt)(_(x))(_(y))) ? Language$Fay$Stdlib$GT : _(_(Fay$$lt)(_(x))(_(y))) ? Language$Fay$Stdlib$LT : Language$Fay$Stdlib$EQ;});};};var Language$Fay$Stdlib$sortBy = function($p1){return new $(function(){var cmp = $p1;return _(_(Language$Fay$Stdlib$foldr)(_(Language$Fay$Stdlib$insertBy)(cmp)))(null);});};var Language$Fay$Stdlib$insertBy = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var x = $p2;return Fay$$list([x]);}var ys = $p3;var x = $p2;var cmp = $p1;return (function($tmp1){if (_($tmp1) === null) {return Fay$$list([x]);}var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var y = $tmp2.car;var ys$39$ = $tmp2.cdr;return (function($tmp2){if (_($tmp2) instanceof $_Language$Fay$Stdlib$GT) {return _(_(Fay$$cons)(y))(_(_(_(Language$Fay$Stdlib$insertBy)(cmp))(x))(ys$39$));}return _(_(Fay$$cons)(x))(ys);})(_(_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$tmp1]); })();})(ys);});};};};var Language$Fay$Stdlib$when = function($p1){return function($p2){return new $(function(){var m = $p2;var p = $p1;return _(p) ? _(_(Fay$$then)(m))(_(Fay$$$_return)(Fay$$unit)) : _(Fay$$$_return)(Fay$$unit);});};};var Language$Fay$Stdlib$succ = function($p1){return new $(function(){var x = $p1;return _(Fay$$add)(_(x))(1);});};var Language$Fay$Stdlib$pred = function($p1){return new $(function(){var x = $p1;return _(Fay$$sub)(_(x))(1);});};var Language$Fay$Stdlib$enumFrom = function($p1){return new $(function(){var i = $p1;return _(_(Fay$$cons)(i))(_(Language$Fay$Stdlib$enumFrom)(_(Fay$$add)(_(i))(1)));});};var Language$Fay$Stdlib$enumFromTo = function($p1){return function($p2){return new $(function(){var n = $p2;var i = $p1;return _(_(Fay$$gt)(_(i))(_(n))) ? null : _(_(Fay$$cons)(i))(_(_(Language$Fay$Stdlib$enumFromTo)(_(Fay$$add)(_(i))(1)))(n));});};};var Language$Fay$Stdlib$enumFromBy = function($p1){return function($p2){return new $(function(){var by = $p2;var fr = $p1;return _(_(Fay$$cons)(fr))(_(_(Language$Fay$Stdlib$enumFromBy)(_(Fay$$add)(_(fr))(_(by))))(by));});};};var Language$Fay$Stdlib$enumFromThen = function($p1){return function($p2){return new $(function(){var th = $p2;var fr = $p1;return _(_(Language$Fay$Stdlib$enumFromBy)(fr))(_(Fay$$sub)(_(th))(_(fr)));});};};var Language$Fay$Stdlib$enumFromByTo = function($p1){return function($p2){return function($p3){return new $(function(){var to = $p3;var by = $p2;var fr = $p1;return (function(){var neg = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$lt)(_(x))(_(to))) ? null : _(_(Fay$$cons)(x))(_(neg)(_(Fay$$add)(_(x))(_(by))));});};var pos = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$gt)(_(x))(_(to))) ? null : _(_(Fay$$cons)(x))(_(pos)(_(Fay$$add)(_(x))(_(by))));});};return _(_(Fay$$lt)(_(by))(0)) ? _(neg)(fr) : _(pos)(fr);})();});};};};var Language$Fay$Stdlib$enumFromThenTo = function($p1){return function($p2){return function($p3){return new $(function(){var to = $p3;var th = $p2;var fr = $p1;return _(_(_(Language$Fay$Stdlib$enumFromByTo)(fr))(_(Fay$$sub)(_(th))(_(fr))))(to);});};};};var Language$Fay$Stdlib$zipWith = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(_(f)(a))(b)))(_(_(_(Language$Fay$Stdlib$zipWith)(f))(as))(bs));}}return null;});};};};var Language$Fay$Stdlib$zipWith3 = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){var $tmp1 = _($p4);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;var f = $p1;return _(_(Fay$$cons)(_(_(_(f)(a))(b))(c)))(_(_(_(_(Language$Fay$Stdlib$zipWith3)(f))(as))(bs))(cs));}}}return null;});};};};};var Language$Fay$Stdlib$zip = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b])))(_(_(Language$Fay$Stdlib$zip)(as))(bs));}}return null;});};};var Language$Fay$Stdlib$zip3 = function($p1){return function($p2){return function($p3){return new $(function(){var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var b = $tmp1.car;var bs = $tmp1.cdr;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var a = $tmp1.car;var as = $tmp1.cdr;return _(_(Fay$$cons)(Fay$$list([a,b,c])))(_(_(_(Language$Fay$Stdlib$zip3)(as))(bs))(cs));}}}return null;});};};};var Language$Fay$Stdlib$unzip = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),2)) {var x = Fay$$index(0)(_($tmp1.car));var y = Fay$$index(1)(_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var xs = Fay$$index(0)(_($tmp1));var ys = Fay$$index(1)(_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(xs),_(_(Fay$$cons)(y))(ys)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(Language$Fay$Stdlib$unzip)(ps));}}if (_($p1) === null) {return Fay$$list([null,null]);}throw ["unhandled case in unzip",[$p1]];});};var Language$Fay$Stdlib$unzip3 = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),3)) {var x = Fay$$index(0)(_($tmp1.car));var y = Fay$$index(1)(_($tmp1.car));var z = Fay$$index(2)(_($tmp1.car));var ps = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(_($tmp1),3)) {var xs = Fay$$index(0)(_($tmp1));var ys = Fay$$index(1)(_($tmp1));var zs = Fay$$index(2)(_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(xs),_(_(Fay$$cons)(y))(ys),_(_(Fay$$cons)(z))(zs)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(Language$Fay$Stdlib$unzip3)(ps));}}if (_($p1) === null) {return Fay$$list([null,null,null]);}throw ["unhandled case in unzip3",[$p1]];});};var Language$Fay$Stdlib$lines = function($p1){return new $(function(){if (_($p1) === null) {return null;}var s = $p1;return (function(){var isLineBreak = function($p1){return new $(function(){var c = $p1;return _(Fay$$or)(_(_(_(Fay$$eq)(c))("\r")))(_(_(_(Fay$$eq)(c))("\n")));});};return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0)(_($tmp1));if (_(Fay$$index(1)(_($tmp1))) === null) {return Fay$$list([a]);}var a = Fay$$index(0)(_($tmp1));var $tmp2 = _(Fay$$index(1)(_($tmp1)));if ($tmp2 instanceof Fay$$Cons) {var cs = $tmp2.cdr;return _(_(Fay$$cons)(a))(_(Language$Fay$Stdlib$lines)(cs));}}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$$_break)(isLineBreak))(s));})();});};var Language$Fay$Stdlib$unlines = new $(function(){return _(Language$Fay$Stdlib$intercalate)(Fay$$list("\n"));});var Language$Fay$Stdlib$words = function($p1){return new $(function(){var str = $p1;return (function(){var words$39$ = function($p1){return new $(function(){if (_($p1) === null) {return null;}var s = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0)(_($tmp1));var b = Fay$$index(1)(_($tmp1));return _(_(Fay$$cons)(a))(_(Language$Fay$Stdlib$words)(b));}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$$_break)(isSpace))(s));});};var isSpace = function($p1){return new $(function(){var c = $p1;return _(_(Language$Fay$Stdlib$elem)(c))(Fay$$list(" \t\r\n\u000c\u000b"));});};return _(words$39$)(_(_(Language$Fay$Stdlib$dropWhile)(isSpace))(str));})();});};var Language$Fay$Stdlib$unwords = new $(function(){return _(Language$Fay$Stdlib$intercalate)(Fay$$list(" "));});var Language$Fay$Stdlib$flip = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var f = $p1;return _(_(f)(y))(x);});};};};var Language$Fay$Stdlib$maybe = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) instanceof $_Language$Fay$Stdlib$Nothing) {var m = $p1;return m;}if (_($p3) instanceof $_Language$Fay$Stdlib$Just) {var x = _($p3).slot1;var f = $p2;return _(f)(x);}throw ["unhandled case in maybe",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$$46$ = function($p1){return function($p2){return function($p3){return new $(function(){var x = $p3;var g = $p2;var f = $p1;return _(f)(_(g)(x));});};};};var Language$Fay$Stdlib$$43$$43$ = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(Language$Fay$Stdlib$conc)(x))(y);});};};var Language$Fay$Stdlib$$36$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(f)(x);});};};var Language$Fay$Stdlib$conc = function($p1){return function($p2){return new $(function(){var ys = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$conc)(xs))(ys));}var ys = $p2;if (_($p1) === null) {return ys;}throw ["unhandled case in conc",[$p1,$p2]];});};};var Language$Fay$Stdlib$concat = new $(function(){return _(_(Language$Fay$Stdlib$foldr)(Language$Fay$Stdlib$conc))(null);});var Language$Fay$Stdlib$concatMap = function($p1){return new $(function(){var f = $p1;return _(_(Language$Fay$Stdlib$foldr)(_(_(Language$Fay$Stdlib$$46$)(Language$Fay$Stdlib$$43$$43$))(f)))(null);});};var Language$Fay$Stdlib$foldr = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(f)(x))(_(_(_(Language$Fay$Stdlib$foldr)(f))(z))(xs));}throw ["unhandled case in foldr",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$foldr1 = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),1)) {var x = Fay$$index(0)(_($p2));return x;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(f)(x))(_(_(Language$Fay$Stdlib$foldr1)(f))(xs));}if (_($p2) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("foldr1: empty list"));}throw ["unhandled case in foldr1",[$p1,$p2]];});};};var Language$Fay$Stdlib$foldl = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return z;}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return _(_(_(Language$Fay$Stdlib$foldl)(f))(_(_(f)(z))(x)))(xs);}throw ["unhandled case in foldl",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$foldl1 = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(_(Language$Fay$Stdlib$foldl)(f))(x))(xs);}if (_($p2) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("foldl1: empty list"));}throw ["unhandled case in foldl1",[$p1,$p2]];});};};var Language$Fay$Stdlib$and = function($p1){return new $(function(){if (_($p1) === null) {return true;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(Fay$$and)(_(x))(_(_(Language$Fay$Stdlib$and)(xs)));}throw ["unhandled case in and",[$p1]];});};var Language$Fay$Stdlib$or = function($p1){return new $(function(){if (_($p1) === null) {return false;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(Fay$$or)(_(x))(_(_(Language$Fay$Stdlib$or)(xs)));}throw ["unhandled case in or",[$p1]];});};var Language$Fay$Stdlib$any = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return false;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(Fay$$or)(_(_(p)(x)))(_(_(_(Language$Fay$Stdlib$any)(p))(xs)));}throw ["unhandled case in any",[$p1,$p2]];});};};var Language$Fay$Stdlib$all = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return true;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(Fay$$and)(_(_(p)(x)))(_(_(_(Language$Fay$Stdlib$all)(p))(xs)));}throw ["unhandled case in all",[$p1,$p2]];});};};var Language$Fay$Stdlib$maximum = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("maximum: empty list"));}var xs = $p1;return _(_(Language$Fay$Stdlib$foldl1)(Language$Fay$Stdlib$max))(xs);});};var Language$Fay$Stdlib$minimum = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("minimum: empty list"));}var xs = $p1;return _(_(Language$Fay$Stdlib$foldl1)(Language$Fay$Stdlib$min))(xs);});};var Language$Fay$Stdlib$product = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("product: empty list"));}var xs = $p1;return _(_(_(Language$Fay$Stdlib$foldl)(Fay$$mult))(1))(xs);});};var Language$Fay$Stdlib$sum = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("sum: empty list"));}var xs = $p1;return _(_(_(Language$Fay$Stdlib$foldl)(Fay$$add))(0))(xs);});};var Language$Fay$Stdlib$scanl = function($p1){return function($p2){return function($p3){return new $(function(){var l = $p3;var z = $p2;var f = $p1;return _(_(Fay$$cons)(z))((function($tmp1){if (_($tmp1) === null) {return null;}var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var x = $tmp2.car;var xs = $tmp2.cdr;return _(_(_(Language$Fay$Stdlib$scanl)(f))(_(_(f)(z))(x)))(xs);}return (function(){ throw (["unhandled case",$tmp1]); })();})(l));});};};};var Language$Fay$Stdlib$scanl1 = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return _(_(_(Language$Fay$Stdlib$scanl)(f))(x))(xs);}throw ["unhandled case in scanl1",[$p1,$p2]];});};};var Language$Fay$Stdlib$scanr = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p3) === null) {var z = $p2;return Fay$$list([z]);}var $tmp1 = _($p3);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var z = $p2;var f = $p1;return (function($tmp1){var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return _(_(Fay$$cons)(_(_(f)(x))(h)))(_(_(Fay$$cons)(h))(t));}return Language$Fay$Stdlib$$_undefined;})(_(_(_(Language$Fay$Stdlib$scanr)(f))(z))(xs));}throw ["unhandled case in scanr",[$p1,$p2,$p3]];});};};};var Language$Fay$Stdlib$scanr1 = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}if (Fay$$listLen(_($p2),1)) {var x = Fay$$index(0)(_($p2));return Fay$$list([x]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var f = $p1;return (function($tmp1){var $tmp2 = _($tmp1);if ($tmp2 instanceof Fay$$Cons) {var h = $tmp2.car;var t = $tmp2.cdr;return _(_(Fay$$cons)(_(_(f)(x))(h)))(_(_(Fay$$cons)(h))(t));}return Language$Fay$Stdlib$$_undefined;})(_(_(Language$Fay$Stdlib$scanr1)(f))(xs));}throw ["unhandled case in scanr1",[$p1,$p2]];});};};var Language$Fay$Stdlib$lookup = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {var _key = $p1;return Language$Fay$Stdlib$Nothing;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {if (Fay$$listLen(_($tmp1.car),2)) {var x = Fay$$index(0)(_($tmp1.car));var y = Fay$$index(1)(_($tmp1.car));var xys = $tmp1.cdr;var key = $p1;return _(_(_(Fay$$eq)(key))(x)) ? _(Language$Fay$Stdlib$Just)(y) : _(_(Language$Fay$Stdlib$lookup)(key))(xys);}}throw ["unhandled case in lookup",[$p1,$p2]];});};};var Language$Fay$Stdlib$intersperse = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$prependToAll)(sep))(xs));}throw ["unhandled case in intersperse",[$p1,$p2]];});};};var Language$Fay$Stdlib$prependToAll = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var sep = $p1;return _(_(Fay$$cons)(sep))(_(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$prependToAll)(sep))(xs)));}throw ["unhandled case in prependToAll",[$p1,$p2]];});};};var Language$Fay$Stdlib$intercalate = function($p1){return function($p2){return new $(function(){var xss = $p2;var xs = $p1;return _(Language$Fay$Stdlib$concat)(_(_(Language$Fay$Stdlib$intersperse)(xs))(xss));});};};var Language$Fay$Stdlib$forM_ = function($p1){return function($p2){return new $(function(){var m = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Fay$$then)(_(m)(x)))(_(_(Language$Fay$Stdlib$forM_)(xs))(m));}if (_($p1) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in forM_",[$p1,$p2]];});};};var Language$Fay$Stdlib$mapM_ = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var m = $p1;return _(_(Fay$$then)(_(m)(x)))(_(_(Language$Fay$Stdlib$mapM_)(m))(xs));}if (_($p2) === null) {return _(Fay$$$_return)(Fay$$unit);}throw ["unhandled case in mapM_",[$p1,$p2]];});};};var Language$Fay$Stdlib$$_const = function($p1){return function($p2){return new $(function(){var a = $p1;return a;});};};var Language$Fay$Stdlib$length = function($p1){return new $(function(){var xs = $p1;return _(_(Language$Fay$Stdlib$length$39$)(0))(xs);});};var Language$Fay$Stdlib$length$39$ = function($p1){return function($p2){return new $(function(){var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var acc = $p1;return _(_(Language$Fay$Stdlib$length$39$)(_(Fay$$add)(_(acc))(1)))(xs);}var acc = $p1;return acc;});};};var Language$Fay$Stdlib$rem = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(_(Fay$$eq)(y))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("Division by zero")) : _(_(Language$Fay$Stdlib$rem$39$)(x))(y);});};};var Language$Fay$Stdlib$rem$39$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["int"],Fay$$fayToJs(["int"],$p1) % Fay$$fayToJs(["int"],$p2));});};};var Language$Fay$Stdlib$quot = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return _(_(_(Fay$$eq)(y))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("Division by zero")) : _(_(Language$Fay$Stdlib$quot$39$)(x))(y);});};};var Language$Fay$Stdlib$quot$39$ = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["int"],~~(Fay$$fayToJs(["int"],$p1)/Fay$$fayToJs(["int"],$p2)));});};};var Language$Fay$Stdlib$quotRem = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;return Fay$$list([_(_(Language$Fay$Stdlib$quot)(x))(y),_(_(Language$Fay$Stdlib$rem)(x))(y)]);});};};var Language$Fay$Stdlib$div = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return _(Fay$$sub)(_(_(_(Language$Fay$Stdlib$quot)(_(Fay$$sub)(_(x))(1)))(y)))(1);} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {return _(Fay$$sub)(_(_(_(Language$Fay$Stdlib$quot)(_(Fay$$add)(_(x))(1)))(y)))(1);}}var y = $p2;var x = $p1;return _(_(Language$Fay$Stdlib$quot)(x))(y);});};};var Language$Fay$Stdlib$mod = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return _(Fay$$add)(_(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$rem)(_(Fay$$sub)(_(x))(1)))(y)))(_(y))))(1);} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(0))))) {return _(Fay$$sub)(_(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$rem)(_(Fay$$add)(_(x))(1)))(y)))(_(y))))(1);}}var y = $p2;var x = $p1;return _(_(Language$Fay$Stdlib$rem)(x))(y);});};};var Language$Fay$Stdlib$divMod = function($p1){return function($p2){return new $(function(){var y = $p2;var x = $p1;if (_(_(Fay$$and)(_(_(Fay$$gt)(_(x))(0)))(_(_(Fay$$lt)(_(y))(0))))) {return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var q = Fay$$index(0)(_($tmp1));var r = Fay$$index(1)(_($tmp1));return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$add)(_(_(Fay$$add)(_(r))(_(y))))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$quotRem)(_(Fay$$sub)(_(x))(1)))(y));} else {if (_(_(Fay$$and)(_(_(Fay$$lt)(_(x))(0)))(_(_(Fay$$gt)(_(y))(1))))) {return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var q = Fay$$index(0)(_($tmp1));var r = Fay$$index(1)(_($tmp1));return Fay$$list([_(Fay$$sub)(_(q))(1),_(Fay$$sub)(_(_(Fay$$add)(_(r))(_(y))))(1)]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$quotRem)(_(Fay$$add)(_(x))(1)))(y));}}var y = $p2;var x = $p1;return _(_(Language$Fay$Stdlib$quotRem)(x))(y);});};};var Language$Fay$Stdlib$min = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.min(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Language$Fay$Stdlib$max = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["unknown"],Math.max(Fay$$fayToJs(["unknown"],$p1),Fay$$fayToJs(["unknown"],$p2)));});};};var Language$Fay$Stdlib$recip = function($p1){return new $(function(){var x = $p1;return _(Fay$$div)(1)(_(x));});};var Language$Fay$Stdlib$fromIntegral = function($p1){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$p1));});};var Language$Fay$Stdlib$otherwise = true;var Language$Fay$Stdlib$reverse = function($p1){return new $(function(){var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;return _(_(Language$Fay$Stdlib$$43$$43$)(_(Language$Fay$Stdlib$reverse)(xs)))(Fay$$list([x]));}if (_($p1) === null) {return null;}throw ["unhandled case in reverse",[$p1]];});};var Language$Fay$Stdlib$$61$$60$$60$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$bind)(x))(f);});};};var Language$Fay$Stdlib$sequence = function($p1){return new $(function(){var ms = $p1;return (function(){var k = function($p1){return function($p2){return new $(function(){var m$39$ = $p2;var m = $p1;return _(_(Fay$$bind)(m))(function($p1){var x = $p1;return _(_(Fay$$bind)(m$39$))(function($p1){var xs = $p1;return _(Fay$$$_return)(_(_(Fay$$cons)(x))(xs));});});});};};return _(_(_(Language$Fay$Stdlib$foldr)(k))(_(Fay$$$_return)(null)))(ms);})();});};var Language$Fay$Stdlib$sequence_ = function($p1){return new $(function(){if (_($p1) === null) {return _(Fay$$$_return)(Fay$$unit);}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var m = $tmp1.car;var ms = $tmp1.cdr;return _(_(Fay$$then)(m))(_(Language$Fay$Stdlib$sequence_)(ms));}throw ["unhandled case in sequence_",[$p1]];});};var Language$Fay$Stdlib$id = function($p1){return new $(function(){var x = $p1;return x;});};var Language$Fay$Stdlib$asTypeOf = new $(function(){return Language$Fay$Stdlib$$_const;});var Language$Fay$Stdlib$until = function($p1){return function($p2){return function($p3){return new $(function(){var x = $p3;var f = $p2;var p = $p1;return _(_(p)(x)) ? x : _(_(_(Language$Fay$Stdlib$until)(p))(f))(_(f)(x));});};};};var Language$Fay$Stdlib$$36$$33$ = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$seq)(x))(_(f)(x));});};};var Language$Fay$Stdlib$$33$$33$ = function($p1){return function($p2){return new $(function(){var b = $p2;var a = $p1;return (function(){var go = function($p1){return function($p2){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("(!!): index too large"));}if (_($p2) === 0) {var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}}var n = $p2;var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return _(_(go)(t))(_(Fay$$sub)(_(n))(1));}throw ["unhandled case in go",[$p1,$p2]];});};};return _(_(Fay$$lt)(_(b))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("(!!): negative index")) : _(_(go)(a))(b);})();});};};var Language$Fay$Stdlib$head = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("head: empty list"));}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;return h;}throw ["unhandled case in head",[$p1]];});};var Language$Fay$Stdlib$tail = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("tail: empty list"));}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return t;}throw ["unhandled case in tail",[$p1]];});};var Language$Fay$Stdlib$init = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("init: empty list"));}if (Fay$$listLen(_($p1),1)) {var a = Fay$$index(0)(_($p1));return Fay$$list([a]);}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var h = $tmp1.car;var t = $tmp1.cdr;return _(_(Fay$$cons)(h))(_(Language$Fay$Stdlib$init)(t));}throw ["unhandled case in init",[$p1]];});};var Language$Fay$Stdlib$last = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("last: empty list"));}if (Fay$$listLen(_($p1),1)) {var a = Fay$$index(0)(_($p1));return a;}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var t = $tmp1.cdr;return _(Language$Fay$Stdlib$last)(t);}throw ["unhandled case in last",[$p1]];});};var Language$Fay$Stdlib$iterate = function($p1){return function($p2){return new $(function(){var x = $p2;var f = $p1;return _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$iterate)(f))(_(f)(x)));});};};var Language$Fay$Stdlib$repeat = function($p1){return new $(function(){var x = $p1;return _(_(Fay$$cons)(x))(_(Language$Fay$Stdlib$repeat)(x));});};var Language$Fay$Stdlib$replicate = function($p1){return function($p2){return new $(function(){if (_($p1) === 0) {return null;}var x = $p2;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("replicate: negative length")) : _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$replicate)(_(Fay$$sub)(_(n))(1)))(x));});};};var Language$Fay$Stdlib$cycle = function($p1){return new $(function(){if (_($p1) === null) {return _(Language$Fay$Stdlib$error)(Fay$$list("cycle: empty list"));}var xs = $p1;return (function(){var xs$39$ = new $(function(){return _(_(Language$Fay$Stdlib$$43$$43$)(xs))(xs$39$);});return xs$39$;})();});};var Language$Fay$Stdlib$take = function($p1){return function($p2){return new $(function(){if (_($p1) === 0) {return null;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("take: negative length")) : _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$take)(_(Fay$$sub)(_(n))(1)))(xs));}throw ["unhandled case in take",[$p1,$p2]];});};};var Language$Fay$Stdlib$drop = function($p1){return function($p2){return new $(function(){var xs = $p2;if (_($p1) === 0) {return xs;}if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("drop: negative length")) : _(_(Language$Fay$Stdlib$drop)(_(Fay$$sub)(_(n))(1)))(xs);}throw ["unhandled case in drop",[$p1,$p2]];});};};var Language$Fay$Stdlib$splitAt = function($p1){return function($p2){return new $(function(){var xs = $p2;if (_($p1) === 0) {return Fay$$list([null,xs]);}if (_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var n = $p1;return _(_(Fay$$lt)(_(n))(0)) ? _(Language$Fay$Stdlib$error)(Fay$$list("splitAt: negative length")) : (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0)(_($tmp1));var b = Fay$$index(1)(_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$splitAt)(_(Fay$$sub)(_(n))(1)))(xs));}throw ["unhandled case in splitAt",[$p1,$p2]];});};};var Language$Fay$Stdlib$takeWhile = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(Language$Fay$Stdlib$takeWhile)(p))(xs)) : null;}throw ["unhandled case in takeWhile",[$p1,$p2]];});};};var Language$Fay$Stdlib$dropWhile = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return null;}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? _(_(Language$Fay$Stdlib$dropWhile)(p))(xs) : _(_(Fay$$cons)(x))(xs);}throw ["unhandled case in dropWhile",[$p1,$p2]];});};};var Language$Fay$Stdlib$span = function($p1){return function($p2){return new $(function(){if (_($p2) === null) {return Fay$$list([null,null]);}var $tmp1 = _($p2);if ($tmp1 instanceof Fay$$Cons) {var x = $tmp1.car;var xs = $tmp1.cdr;var p = $p1;return _(_(p)(x)) ? (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var a = Fay$$index(0)(_($tmp1));var b = Fay$$index(1)(_($tmp1));return Fay$$list([_(_(Fay$$cons)(x))(a),b]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(_(Language$Fay$Stdlib$span)(p))(xs)) : Fay$$list([null,_(_(Fay$$cons)(x))(xs)]);}throw ["unhandled case in span",[$p1,$p2]];});};};var Language$Fay$Stdlib$$_break = function($p1){return new $(function(){var p = $p1;return _(Language$Fay$Stdlib$span)(_(_(Language$Fay$Stdlib$$46$)(Language$Fay$Stdlib$not))(p));});};var Language$Fay$Stdlib$print = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["unknown"],$p1)));});};var Language$Fay$Stdlib$putStrLn = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],(function(x) { if (console && console.log) console.log(x) })(Fay$$fayToJs(["string"],$p1)));});};var $_Language$Fay$Stdlib$Just = function(slot1){this.slot1 = slot1;};var Language$Fay$Stdlib$Just = function(slot1){return new $(function(){return new $_Language$Fay$Stdlib$Just(slot1);});};var $_Language$Fay$Stdlib$Nothing = function(){};var Language$Fay$Stdlib$Nothing = new $(function(){return new $_Language$Fay$Stdlib$Nothing();});var Optimisation$CirclePacking$comparing = function($p1){return function($p2){return function($p3){return new $(function(){var y = $p3;var x = $p2;var p = $p1;return _(_(Language$Fay$Stdlib$compare)(_(p)(x)))(_(p)(y));});};};};var Optimisation$CirclePacking$fromMaybe = function($p1){return function($p2){return new $(function(){var x = $p2;var d = $p1;return (function($tmp1){if (_($tmp1) instanceof $_Language$Fay$Stdlib$Nothing) {return d;}if (_($tmp1) instanceof $_Language$Fay$Stdlib$Just) {var v = _($tmp1).slot1;return v;}return (function(){ throw (["unhandled case",$tmp1]); })();})(x);});};};var Optimisation$CirclePacking$packCircles = function($p1){return new $(function(){var radiusFunction = $p1;return _(_(Language$Fay$Stdlib$$46$)(_(Language$Fay$Stdlib$map)(function($p1){var t = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var x = Fay$$index(0)(_($tmp1));var p = Fay$$index(1)(_($tmp1));return Fay$$list([_(Language$Fay$Stdlib$snd)(x),p]);}return (function(){ throw (["unhandled case",$tmp1]); })();})(t);})))(_(_(Language$Fay$Stdlib$$46$)(Optimisation$CirclePacking$centerUp))(_(_(Language$Fay$Stdlib$$46$)(Language$Fay$Stdlib$fst))(_(_(Language$Fay$Stdlib$$46$)(Optimisation$CirclePacking$go))(_(_(Language$Fay$Stdlib$$46$)(_(Language$Fay$Stdlib$sortBy)(_(Optimisation$CirclePacking$comparing)(Optimisation$CirclePacking$radius))))(_(Language$Fay$Stdlib$map)(function($p1){var x = $p1;return Fay$$list([_(radiusFunction)(x),x]);}))))));});};var Optimisation$CirclePacking$radius = new $(function(){return Language$Fay$Stdlib$fst;});var Optimisation$CirclePacking$go = function($p1){return new $(function(){if (_($p1) === null) {return Fay$$list([null,null]);}var $tmp1 = _($p1);if ($tmp1 instanceof Fay$$Cons) {var c = $tmp1.car;var cs = $tmp1.cdr;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {var placed = Fay$$index(0)(_($tmp1));var pairs = Fay$$index(1)(_($tmp1));return (function($tmp2){if (Fay$$listLen(_($tmp2),2)) {var cp = Fay$$index(0)(_($tmp2));var newPairs = Fay$$index(1)(_($tmp2));return Fay$$list([_(_(Fay$$cons)(cp))(placed),_(_(Language$Fay$Stdlib$$43$$43$)(newPairs))(pairs)]);}return (function(){ throw (["unhandled case",$tmp2]); })();})(_(_(_(Optimisation$CirclePacking$place)(c))(placed))(pairs));}return (function(){ throw (["unhandled case",$tmp1]); })();})(_(Optimisation$CirclePacking$go)(cs));}throw ["unhandled case in go",[$p1]];});};var Optimisation$CirclePacking$place = function($p1){return function($p2){return function($p3){return new $(function(){if (_($p2) === null) {var c = $p1;return Fay$$list([Fay$$list([c,Fay$$list([0,0])]),null]);}if (Fay$$listLen(_($p2),1)) {var cp$39$ = Fay$$index(0)(_($p2));if (Fay$$listLen(_(Fay$$index(0)(_($p2))),2)) {var c$39$ = Fay$$index(0)(_(Fay$$index(0)(_($p2))));if (Fay$$listLen(_(Fay$$index(1)(_(Fay$$index(0)(_($p2))))),2)) {var x = Fay$$index(0)(_(Fay$$index(1)(_(Fay$$index(0)(_($p2))))));var y = Fay$$index(1)(_(Fay$$index(1)(_(Fay$$index(0)(_($p2))))));var c = $p1;return (function(){var cp = new $(function(){return Fay$$list([c,Fay$$list([_(Fay$$add)(_(_(Fay$$add)(_(x))(_(_(Optimisation$CirclePacking$radius)(c)))))(_(_(Optimisation$CirclePacking$radius)(c$39$))),y])]);});return Fay$$list([cp,Fay$$list([Fay$$list([cp,cp$39$])])]);})();}}var c = $p1;return (function(){var cp = new $(function(){return Fay$$list([c,Fay$$list([_(Fay$$add)(_(_(Fay$$add)(_(x))(_(_(Optimisation$CirclePacking$radius)(c)))))(_(_(Optimisation$CirclePacking$radius)(c$39$))),y])]);});return Fay$$list([cp,Fay$$list([Fay$$list([cp,cp$39$])])]);})();}var pairs = $p3;var placed = $p2;var c = $p1;return (function(){var newPairs = new $(function(){var $gen_1 = function($p1){return new $(function(){var cp$39$ = $p1;return _(_(_(touching)(cp))(cp$39$)) ? Fay$$list([Fay$$list([cp,cp$39$])]) : null;return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);});var cp = new $(function(){return Fay$$list([c,p]);});var p = new $(function(){return _(_(Language$Fay$Stdlib$$36$)(_(Optimisation$CirclePacking$fromMaybe)(_(Language$Fay$Stdlib$error)(Fay$$list("packCircles: The end of the real plane has been reached?")))))(_(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$find)(function($p1){var p$39$ = $p1;return _(_(Language$Fay$Stdlib$all)(_(valid)(p$39$)))(placed);})))(_(_(Language$Fay$Stdlib$sortBy)(_(Optimisation$CirclePacking$comparing)(centerDistance)))((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c1 = Fay$$index(0)(_($p1));var c2 = Fay$$index(1)(_($p1));return _(_(_(touching)(c1))(c2)) ? (function(){var $gen_1 = function($p1){return new $(function(){var p$39$ = $p1;return Fay$$list([p$39$]);return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(_(_(near)(c1))(c2));})() : null;}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(pairs);})())));});var centerDistance = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var x = Fay$$index(0)(_($p1));var y = Fay$$index(1)(_($p1));return _(Language$Fay$Stdlib$sqrt)(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(centerx))(_(x))))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(centery))(_(y))))(2))));}throw ["unhandled case in centerDistance",[$p1]];});};var centerx = new $(function(){return _(Fay$$div)(_(_(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var x = Fay$$index(0)(_(Fay$$index(1)(_($p1))));return Fay$$list([_(Fay$$mult)(_(x))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)))]);}}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})())))(_(area));});var centery = new $(function(){return _(Fay$$div)(_(_(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var y = Fay$$index(1)(_(Fay$$index(1)(_($p1))));return Fay$$list([_(Fay$$mult)(_(y))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)))]);}}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})())))(_(area));});var area = new $(function(){return _(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));return Fay$$list([_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)]);}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})());});var valid = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var c2 = Fay$$index(0)(_($p2));if (Fay$$listLen(_(Fay$$index(1)(_($p2))),2)) {var x2 = Fay$$index(0)(_(Fay$$index(1)(_($p2))));var y2 = Fay$$index(1)(_(Fay$$index(1)(_($p2))));if (Fay$$listLen(_($p1),2)) {var x1 = Fay$$index(0)(_($p1));var y1 = Fay$$index(1)(_($p1));return _(_(Fay$$gte)(_(Language$Fay$Stdlib$sqrt)(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(x2))(_(x1))))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(y2))(_(y1))))(2))))))(_(Fay$$mult)(_(_(Fay$$add)(_(_(Optimisation$CirclePacking$radius)(c)))(_(_(Optimisation$CirclePacking$radius)(c2)))))(_(_(Fay$$sub)(1)(_(Optimisation$CirclePacking$eps)))));}}}throw ["unhandled case in valid",[$p1,$p2]];});};};var touching = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var c2 = Fay$$index(0)(_($p2));if (Fay$$listLen(_(Fay$$index(1)(_($p2))),2)) {var x2 = Fay$$index(0)(_(Fay$$index(1)(_($p2))));var y2 = Fay$$index(1)(_(Fay$$index(1)(_($p2))));if (Fay$$listLen(_($p1),2)) {var c1 = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var x1 = Fay$$index(0)(_(Fay$$index(1)(_($p1))));var y1 = Fay$$index(1)(_(Fay$$index(1)(_($p1))));return _(_(Fay$$lte)(_(Language$Fay$Stdlib$sqrt)(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(x2))(_(x1))))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(y2))(_(y1))))(2))))))(_(Fay$$mult)(_(_(Fay$$add)(_(_(Optimisation$CirclePacking$radius)(c1)))(_(_(Optimisation$CirclePacking$radius)(c2)))))(_(_(Fay$$add)(1)(_(Optimisation$CirclePacking$eps)))));}}}}throw ["unhandled case in touching",[$p1,$p2]];});};};var near = function($p1){return function($p2){return new $(function(){if (Fay$$listLen(_($p2),2)) {var c2 = Fay$$index(0)(_($p2));if (Fay$$listLen(_(Fay$$index(1)(_($p2))),2)) {var x2 = Fay$$index(0)(_(Fay$$index(1)(_($p2))));var y2 = Fay$$index(1)(_(Fay$$index(1)(_($p2))));if (Fay$$listLen(_($p1),2)) {var c1 = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var x1 = Fay$$index(0)(_(Fay$$index(1)(_($p1))));var y1 = Fay$$index(1)(_(Fay$$index(1)(_($p1))));return (function(){var base = new $(function(){return _(Language$Fay$Stdlib$sqrt)(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(x2))(_(x1))))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Fay$$sub)(_(y2))(_(y1))))(2))));});var lat1 = new $(function(){return _(Fay$$add)(_(_(Optimisation$CirclePacking$radius)(c1)))(_(_(Optimisation$CirclePacking$radius)(c)));});var lat2 = new $(function(){return _(Fay$$add)(_(_(Optimisation$CirclePacking$radius)(c2)))(_(_(Optimisation$CirclePacking$radius)(c)));});var ad_length = new $(function(){return _(Fay$$div)(_(_(Fay$$sub)(_(_(Fay$$add)(_(_(_(Language$Fay$Stdlib$$42$$42$)(base))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(lat1))(2)))))(_(_(_(Language$Fay$Stdlib$$42$$42$)(lat2))(2)))))(_(_(Fay$$mult)(2)(_(base))));});var h = new $(function(){return _(Language$Fay$Stdlib$sqrt)(_(Language$Fay$Stdlib$abs)(_(Fay$$sub)(_(_(_(Language$Fay$Stdlib$$42$$42$)(lat1))(2)))(_(_(_(Language$Fay$Stdlib$$42$$42$)(ad_length))(2)))));});var dx = new $(function(){return _(Fay$$add)(_(x1))(_(_(Fay$$div)(_(_(Fay$$mult)(_(ad_length))(_(_(Fay$$sub)(_(x2))(_(x1))))))(_(base))));});var dy = new $(function(){return _(Fay$$add)(_(y1))(_(_(Fay$$div)(_(_(Fay$$mult)(_(ad_length))(_(_(Fay$$sub)(_(y2))(_(y1))))))(_(base))));});var c1x = new $(function(){return _(Fay$$add)(_(dx))(_(_(Fay$$div)(_(_(Fay$$mult)(_(h))(_(_(Fay$$sub)(_(y2))(_(y1))))))(_(base))));});var c1y = new $(function(){return _(Fay$$sub)(_(dy))(_(_(Fay$$div)(_(_(Fay$$mult)(_(h))(_(_(Fay$$sub)(_(x2))(_(x1))))))(_(base))));});var c2x = new $(function(){return _(Fay$$sub)(_(dx))(_(_(Fay$$div)(_(_(Fay$$mult)(_(h))(_(_(Fay$$sub)(_(y2))(_(y1))))))(_(base))));});var c2y = new $(function(){return _(Fay$$add)(_(dy))(_(_(Fay$$div)(_(_(Fay$$mult)(_(h))(_(_(Fay$$sub)(_(x2))(_(x1))))))(_(base))));});return Fay$$list([Fay$$list([c1x,c1y]),Fay$$list([c2x,c2y])]);})();}}}}throw ["unhandled case in near",[$p1,$p2]];});};};return Fay$$list([cp,newPairs]);})();});};};};var Optimisation$CirclePacking$centerUp = function($p1){return new $(function(){var placed = $p1;return (function(){var centerx = new $(function(){return _(Fay$$div)(_(_(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var x = Fay$$index(0)(_(Fay$$index(1)(_($p1))));return Fay$$list([_(Fay$$mult)(_(x))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)))]);}}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})())))(_(area));});var centery = new $(function(){return _(Fay$$div)(_(_(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var y = Fay$$index(1)(_(Fay$$index(1)(_($p1))));return Fay$$list([_(Fay$$mult)(_(y))(_(_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)))]);}}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})())))(_(area));});var area = new $(function(){return _(Language$Fay$Stdlib$sum)((function(){var $gen_1 = function($p1){return new $(function(){if (Fay$$listLen(_($p1),2)) {var c$39$ = Fay$$index(0)(_($p1));return Fay$$list([_(_(Language$Fay$Stdlib$$42$$42$)(_(Optimisation$CirclePacking$radius)(c$39$)))(2)]);}return null;});};return _(_(Language$Fay$Stdlib$concatMap)($gen_1))(placed);})());});return _(_(Language$Fay$Stdlib$map)(function($p1){if (Fay$$listLen(_($p1),2)) {var o = Fay$$index(0)(_($p1));if (Fay$$listLen(_(Fay$$index(1)(_($p1))),2)) {var x = Fay$$index(0)(_(Fay$$index(1)(_($p1))));var y = Fay$$index(1)(_(Fay$$index(1)(_($p1))));return Fay$$list([o,Fay$$list([_(Fay$$sub)(_(x))(_(centerx)),_(Fay$$sub)(_(y))(_(centery))])]);}}throw ["unhandled case",$p1];}))(placed);})();});};var Optimisation$CirclePacking$eps = 1.0e-5;var FayDemo$main = new $(function(){return _(_(Fay$$bind)(FayDemo$getWindow))(function($p1){var $_window = $p1;return _(_(_(_(FayDemo$addEventListener)($_window))(Fay$$list("load")))(FayDemo$start))(false);});});var FayDemo$colors = new $(function(){return Fay$$list([Fay$$list("Green"),Fay$$list("Silver"),Fay$$list("Lime"),Fay$$list("Gray"),Fay$$list("Olive"),Fay$$list("Yellow"),Fay$$list("Maroon"),Fay$$list("Navy"),Fay$$list("Red"),Fay$$list("Blue"),Fay$$list("Purple"),Fay$$list("Teal"),Fay$$list("Fuchsia"),Fay$$list("Aqua")]);});var FayDemo$parseInt = function($p1){return new $(function(){return Fay$$jsToFay(["int"],(function () { var n = window.parseInt(Fay$$fayToJs(["string"],$p1), 10); if (isNaN(n)) return null; return n; })());});};var FayDemo$start = new $(function(){return _(_(Fay$$bind)(_(FayDemo$getElementById)(Fay$$list("input"))))(function($p1){var input = $p1;return _(_(Fay$$then)(_(_(_(_(FayDemo$addEventListener)(input))(Fay$$list("change")))(FayDemo$update))(false)))(_(_(Fay$$then)(_(_(_(_(FayDemo$addEventListener)(input))(Fay$$list("input")))(FayDemo$update))(false)))(_(_(Fay$$then)(_(_(_(_(FayDemo$addEventListener)(input))(Fay$$list("keyup")))(FayDemo$update))(false)))(_(_(Fay$$then)(_(_(_(_(FayDemo$addEventListener)(input))(Fay$$list("paste")))(FayDemo$update))(false)))(FayDemo$update))));});});var FayDemo$update = new $(function(){return _(_(Fay$$bind)(_(FayDemo$getElementById)(Fay$$list("can"))))(function($p1){var canvas = $p1;return _(_(Fay$$then)(_(FayDemo$clearCanvas)(canvas)))(_(_(Fay$$bind)(_(_(FayDemo$getContext)(canvas))(Fay$$list("2d"))))(function($p1){var context = $p1;return _(_(Fay$$bind)(_(FayDemo$getWidth)(canvas)))(function($p1){var w = $p1;return _(_(Fay$$bind)(_(FayDemo$getHeight)(canvas)))(function($p1){var h = $p1;return _(_(Fay$$bind)(_(FayDemo$getElementById)(Fay$$list("input"))))(function($p1){var input = $p1;return _(_(Fay$$bind)(_(FayDemo$getValue)(input)))(function($p1){var value = $p1;return (function(){var radii = new $(function(){return _(_(Language$Fay$Stdlib$map)(_(_(Language$Fay$Stdlib$$46$)(Language$Fay$Stdlib$fromIntegral))(FayDemo$parseInt)))(_(Language$Fay$Stdlib$words)(value));});return (function(){var colored = new $(function(){return _(_(Language$Fay$Stdlib$zip)(radii))(_(Language$Fay$Stdlib$cycle)(FayDemo$colors));});return (function(){var packed = new $(function(){return _(_(Optimisation$CirclePacking$packCircles)(Language$Fay$Stdlib$fst))(colored);});return _(_(Language$Fay$Stdlib$$36$)(_(Language$Fay$Stdlib$forM_)(packed)))(function($p1){var res = $p1;return (function($tmp1){if (Fay$$listLen(_($tmp1),2)) {if (Fay$$listLen(_(Fay$$index(0)(_($tmp1))),2)) {var r = Fay$$index(0)(_(Fay$$index(0)(_($tmp1))));var c = Fay$$index(1)(_(Fay$$index(0)(_($tmp1))));if (Fay$$listLen(_(Fay$$index(1)(_($tmp1))),2)) {var x = Fay$$index(0)(_(Fay$$index(1)(_($tmp1))));var y = Fay$$index(1)(_(Fay$$index(1)(_($tmp1))));return _(_(Fay$$then)(_(FayDemo$beginPath)(context)))(_(_(Fay$$then)(_(_(_(_(FayDemo$addCircle)(context))(r))(_(Fay$$add)(_(x))(_(_(Fay$$div)(_(w))(2)))))(_(Fay$$add)(_(y))(_(_(Fay$$div)(_(h))(2))))))(_(_(Fay$$then)(_(_(FayDemo$setFillStyle)(context))(c)))(_(FayDemo$fill)(context))));}}}return (function(){ throw (["unhandled case",$tmp1]); })();})(res);});})();})();})();});});});});}));});});var FayDemo$addEventListener = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["unknown"],$p1)['addEventListener'](Fay$$fayToJs(["string"],$p2),Fay$$fayToJs(["action",[["unknown"]]],$p3),Fay$$fayToJs(["bool"],$p4)));});};};};};var FayDemo$getElementById = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document['getElementById'](Fay$$fayToJs(["string"],$p1)));});};var FayDemo$getElementsByTagName = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document['getElementsByTagName'](Fay$$fayToJs(["string"],$p1))[0]);});};var FayDemo$newImage = new $(function(){return Fay$$jsToFay(["action",[["user","Image",[]]]],new Image());});var FayDemo$setSrc = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Image",[]],$p1)['src'] = Fay$$fayToJs(["string"],$p2));});};};var FayDemo$getContext = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["user","Context",[]]]],Fay$$fayToJs(["user","Element",[]],$p1)['getContext'](Fay$$fayToJs(["string"],$p2)));});};};var FayDemo$clearCanvas = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Element",[]],$p1)['getContext']('2d')['clearRect']( 0 , 0 , Fay$$fayToJs(["user","Element",[]],$p1)['width'] , Fay$$fayToJs(["user","Element",[]],$p1)['height']));});};var FayDemo$getValue = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","Element",[]],$p1)['value']);});};var FayDemo$getWidth = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Element",[]],$p1)['width']);});};var FayDemo$getHeight = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","Element",[]],$p1)['height']);});};var FayDemo$beginPath = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['beginPath']());});};var FayDemo$addCircle = function($p1){return function($p2){return function($p3){return function($p4){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['arc'](Fay$$fayToJs(["double"],$p3),Fay$$fayToJs(["double"],$p4),Fay$$fayToJs(["double"],$p2),0,2*Math.PI,false));});};};};};var FayDemo$setFillStyle = function($p1){return function($p2){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fillStyle']=Fay$$fayToJs(["string"],$p2));});};};var FayDemo$fill = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],Fay$$fayToJs(["user","Context",[]],$p1)['fill']());});};var FayDemo$getWindow = new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],window);});var FayDemo$alert = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['alert'](Fay$$fayToJs(["unknown"],$p1)));});};var FayDemo$print = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],console['log'](Fay$$fayToJs(["double"],$p1)));});};var FayDemo$printLog = function($p1){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],console['log'](Fay$$fayToJs(["string"],$p1)));});};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = _(obj);var argTypes = type[2];if (_obj instanceof $_Language$Fay$Stdlib$Nothing) {var obj_ = {"instance": "Nothing"};return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$Just) {var obj_ = {"instance": "Just"};var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_.slot1 = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$EQ) {var obj_ = {"instance": "EQ"};return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$LT) {var obj_ = {"instance": "LT"};return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$GT) {var obj_ = {"instance": "GT"};return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$Right) {var obj_ = {"instance": "Right"};var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_.slot1 = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$Left) {var obj_ = {"instance": "Left"};var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_.slot1 = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$Defined) {var obj_ = {"instance": "Defined"};var obj_slot1 = Fay$$fayToJs(["unknown"],_(_obj.slot1));if (undefined !== obj_slot1) {obj_.slot1 = obj_slot1;}return obj_;}if (_obj instanceof $_Language$Fay$Stdlib$Undefined) {var obj_ = {"instance": "Undefined"};return obj_;}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){if (obj["instance"] === "Nothing") {return new $_Language$Fay$Stdlib$Nothing();}if (obj["instance"] === "Just") {return new $_Language$Fay$Stdlib$Just(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "EQ") {return new $_Language$Fay$Stdlib$EQ();}if (obj["instance"] === "LT") {return new $_Language$Fay$Stdlib$LT();}if (obj["instance"] === "GT") {return new $_Language$Fay$Stdlib$GT();}if (obj["instance"] === "Right") {return new $_Language$Fay$Stdlib$Right(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "Left") {return new $_Language$Fay$Stdlib$Left(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "Defined") {return new $_Language$Fay$Stdlib$Defined(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "Undefined") {return new $_Language$Fay$Stdlib$Undefined();}return obj;};
// Exports
this.FayDemo$main = FayDemo$main;

// Built-ins
this._ = _;
this.$           = $;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new FayDemo();
main._(main.FayDemo$main);

