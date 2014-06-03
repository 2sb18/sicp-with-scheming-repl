/* exported evaluate, qeval,  */
// environment stuff
/* exported set_variable_value, lookup_variable_value, extend_environment */
/* exported define_variable, empty_environment */
// eval stuff
/* exported eval_if, eval_lamda */
/* exported expressionToTree */

/*
 * Evaluates an expression string in a specific environment
 *
 * @param  expressionTree an expression turned into an
 *           expression tree by expressionToTree
 *            ex. "(define meow (lambda (x) (* 2 x)))"
 *            ex. ["define", "meow", ["lambda", ["*", "2", "x"]]]
 * @param  env the environment to execute the expression in
 *
 * @return the result of the expression execution
 */
function evaluate(expressionTree, env) {
  "use strict";

  // find out what kind of expression this is
  if (typeof expressionTree === "string") {
    if (!isNaN(parseFloat(expressionTree))) {
      return parseFloat(expressionTree);
    } else if (expressionTree === "true") {
      return true;
    } else if (expressionTree === "false") {
      return false;
    } else {
      var lookup = lookup_variable_value(expressionTree, env);
      if (typeof lookup === "undefined") {
        return expressionTree;
      } else {
        return lookup;
      }
    }
  }

  // we know expressionTree is a list
  // but what kind of expression is it?
  switch (evaluate(expressionTree[0], env)) {
    case "if":
      return eval_if(expressionTree.slice(1), env);
    case "define":
      return eval_define(expressionTree.slice(0), env);
    case "lambda":
      return eval_lambda(expressionTree.slice(0), env);

      // primitives
    case "*":
      return eval_mult(expressionTree.slice(1), env);
    case "/":
      return eval_div(expressionTree.slice(1), env);
    case "+":
      return eval_add(expressionTree.slice(1), env);
    case "-":
      return eval_sub(expressionTree.slice(1), env);
    case "=":
      return eval_equals(expressionTree.slice(1), env);
    default:
      // application
      return apply(expressionTree, env);
  }
}

// for quick-evaluate
// always returns a string
function qeval(expression, env) {
  "use strict";
  if (typeof env === "undefined") {
    throw "called qeval(...) without sending an environment argument";
  }
  var result = evaluate(expressionToTree(expression), env);
  if (typeof result !== "undefined") {
    return result.toString();
  } else {
    return undefined;
  }
}

function eval_mult(expressionTree, env) {
  "use strict";
  return evaluate(expressionTree[0], env) * evaluate(expressionTree[1], env);
}

function eval_div(expressionTree, env) {
  "use strict";
  return evaluate(expressionTree[0], env) / evaluate(expressionTree[1], env);
}

function eval_add(expressionTree, env) {
  "use strict";
  return evaluate(expressionTree[0], env) + evaluate(expressionTree[1], env);
}

function eval_sub(expressionTree, env) {
  "use strict";
  return evaluate(expressionTree[0], env) - evaluate(expressionTree[1], env);
}

function eval_equals(expressionTree, env) {
  "use strict";
  return evaluate(expressionTree[0], env) === evaluate(expressionTree[1], env);
}

function apply(expressionTree, env) {
  "use strict";
  // if expressionTree looks like this: (meow 3 4 7)
  //
  // we gotta find the environment that the procedure points to
  if (typeof expressionTree[0] !== "object") {
    var procedure = lookup_variable_value(expressionTree[0], env);
    // then extend that environment
    var new_environment = extend_environment(procedure.env);
    // and populate it
    var i, j = 1;
    for (i = 0; i < procedure.parameters.length; i++) {
      //                                   should this be new_environment? ---V 
      define_variable(procedure.parameters[i], evaluate(expressionTree[j++], env), new_environment);
    }
    return evaluate(procedure.code, new_environment);
  } else {
    // we're looking at something like
    // ((lambda (x) (* 2 x)) 3)
  }

}

function eval_if(expressionTree, env) {
  "use strict";
  // expressionTree should look like this...
  // (3 1 4)
  if (evaluate(expressionTree[0], env)) {
    return evaluate(expressionTree[1], env);
  } else if (expressionTree.length === 3) {
    return evaluate(expressionTree[2], env);
  } else {
    return false;
  }
}

function eval_define(expressionTree, env) {
  "use strict";
  // expressionTree should look like this...
  // (define variable_name variable_value)
  if (typeof expressionTree[1] !== "object") {
    // do we need to evaluate the variable name in a define?
    define_variable(evaluate(expressionTree[1], env),
      evaluate(expressionTree[2], env), env);
  } else {
    // or this...
    // (define (function_name param1 param2) (* 2 x))
    // which we need to transform into this
    // (define function_name (lambda (param1 param2) (* 2 x)))

    // first, create the lambda
    var lamb = ["lambda"];
    lamb.push(expressionTree[1].slice(1));
    lamb.push(expressionTree[2]);
    define_variable(evaluate(expressionTree[1][0], env),
      evaluate(lamb, env), env);
  }
}


// a procedure should look like this. it should be an object
// procedure.parameters = [x, y, z, etc];
// procedure.code = expressionTree of code
// procedure.env points to the parent_frame
function eval_lambda(expressionTree, env) {
  "use strict";
  // expression Tree should look like this...
  // (lambda (x y) (* x y))
  var procedure = {};
  procedure.parameters = expressionTree[1];
  procedure.code = expressionTree[2];
  procedure.env = env;
  return procedure;
}

/*
 * Our environment structure is like so...
 * each frame in the environment is an object,
 * with a parent_frame element which points to another
 * frame, or is undefined if it's the top element.
 * The frame also has a binding element, which is an
 * array of variables in that frame
 */

/*
 * We're assuming that env is already a well structured
 * environment. ie it's an object that has a parent_frame element
 * and a binding element
 * @param variable: a string that is the variable name
 *
 * adds to the first frame in the environment
 */
function define_variable(variable, val, env) {
  "use strict";
  env.bindings[variable] = val;
}

/* find the variable in the environment and sets it.
 * if variable can't be found, throws an error
 */
function set_variable_value(variable, val, env) {
  "use strict";
  if (typeof env.bindings[variable] !== "undefined") {
    env.bindings[variable] = val;
  } else if (typeof env.parent_frame === "undefined") {
    throw "variable can't be found!";
  } else {
    set_variable_value(variable, val, env.parent_frame);
  }
}

function lookup_variable_value(variable, env) {
  "use strict";
  if (typeof env.bindings[variable] !== "undefined") {
    return env.bindings[variable];
  } else if (typeof env.parent_frame === "undefined") {
    return undefined;
  } else {
    return lookup_variable_value(variable, env.parent_frame);
  }
}

// create an empty frame that extends the env sent to it
function extend_environment(env) {
  "use strict";
  var new_env = {};
  new_env.parent_frame = env;
  new_env.bindings = [];
  return new_env;
}

function empty_environment() {
  "use strict";
  return extend_environment(undefined);
}

// all the nodes in the tree look like this:
// obj = { array: [ obj1, obj2 ] }
//
// turn into a tree of arrays, ie get rid of
// the object part
//
// so...
// array = [ array1, array2]
function convertTreeObjIntoTreeArray(top_obj) {
  "use strict";
  var array_to_return = [];
  if (typeof top_obj.array === "undefined") {
    return top_obj;
  }
  top_obj.array.forEach(function(element) {
    if (typeof element === "object") {
      array_to_return.push(convertTreeObjIntoTreeArray(element));
    } else {
      array_to_return.push(element);
    }
  });
  return array_to_return;
}


/*
 * Turns an expression into an array tree
 *
 * @param  exp a string that represents the input expression
 *
 * @return an array tree representing the expression
 *
 */
function expressionToTree(exp) {
  "use strict";

  // slightly modify the exp
  // exp is wrapped in array so it's easy to mutate it.
  // this is because arrays are passed by reference.
  exp = [exp];

  function lop_one_char() {
    exp = [exp[0].slice(1)];
  }

  // removes the string from exp and returns it

  function consume_string_and_return_it() {
    // the first character is ", so we know the string starts at 1
    // must find end of string
    var end = exp[0].indexOf("\"", 1);
    var string = exp[0].slice(0, end + 1);
    exp = [exp[0].slice(end + 1)];
    return string;
  }

  function consume_symbol_and_return_it() {
    // the first character is ", so we know the string starts at 1
    // must find end of string
    var string;
    var end = exp[0].substr(1).search(/[ \(\)\"]/);
    if (end === -1) {
      string = exp[0];
      exp = [""];
      return string;
    }
    string = exp[0].slice(0, end + 1);
    exp = [exp[0].slice(end + 1)];
    return string;
  }

  var level = 0;

  // downside to how this works: there are a lot of nested
  // calls to expToTree. Worse case: there's a call for
  // every character. May lead to stack overflow?

  // parent_obj looks like parent_obj = { array };

  function expToTree(parent_obj) {
    if (typeof parent_obj === "undefined" || typeof parent_obj.array !== "object") {
      throw "parent_obj is not formatted correctly";
    }

    if (exp[0].length === 0) {
      // we're done!
      return;
    }

    switch (exp[0][0]) {
      case " ":
        // get rid of spaces outside of strings
        lop_one_char();
        expToTree(parent_obj);
        break;
      case "\"": // it's a string!
        parent_obj.array.push(consume_string_and_return_it());
        expToTree(parent_obj);
        break;
      case "(": // it's the start of a list
        level++;
        lop_one_char();
        var new_obj = {
          array: []
        };
        parent_obj.array.push(new_obj);
        expToTree(new_obj); // this should take care of whole list
        expToTree(parent_obj); // now let's get back to the parent
        break;
      case ")": // it's the end of the list
        level--;
        lop_one_char();
        break;
      default: // if it's anything else, take it as a symbol
        parent_obj.array.push(consume_symbol_and_return_it());
        expToTree(parent_obj);
    }
  }

  var top_obj = {
    array: []
  };
  expToTree(top_obj);
  if (level !== 0) {
    throw "malformed list: unclosed brackets";
  }
  return convertTreeObjIntoTreeArray(top_obj.array[0]);
}

/*************
 * we have to deal with array manipulations alot. here's how to do them:
 * concatenation: array1.concat(array2, value3, so on...)
 * sliceofarray: array1.slice(start, end); //end not needed, negative values can be used
 */
