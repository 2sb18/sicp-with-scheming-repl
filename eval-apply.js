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
      return eval_define(expressionTree.slice(1), env);
    case "lambda":
      return eval_lambda(expressionTree.slice(1), env);

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

// expressionTree should look like
// (meow 3 4 7)
function apply(expressionTree, env) {
  "use strict";
  // we gotta find the environment that the procedure points to
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
  // (meow hello)
  define_variable(evaluate(expressionTree[0], env),
    evaluate(expressionTree[1], env), env);
}

// a procedure should look like this. it should be an object
// procedure.parameters = [x, y, z, etc];
// procedure.code = expressionTree of code
// procedure.env points to the parent_frame
function eval_lambda(expressionTree, env) {
  "use strict";
  // expression Tree should look like this...
  // ((x y) (* x y))
  var procedure = {};
  procedure.parameters = expressionTree[0];
  procedure.code = expressionTree[1];
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


/*
 * Turns an expression into an array tree
 *
 * @param  exp a string that represents the input expression
 *
 * @return an array tree representing the expression
 */
function expressionToTree(exp) {
  "use strict";

  // get rid of spaces on the front of the expression
  var i = 0;

  while (1) {
    if (exp[i] === "(") {
      // we're dealing with a list
      break;
    } else if (exp[i] === " ") {
      //do nothing
      // (ignoring cause of empty block above)
    } else { // jshint ignore:line 
      if (-1 !== exp.search(/\(|\)/)) {
        throw "expression that didn't start with ( has either ( or ) in it";
      }
      return exp.slice(i);
    }
    i++;
  }

  // at this point, we know we're dealing with a list
  var array_to_return = [];
  var start;
  var level = 0;
  var state = "in_between";


  // at this point, we know we're dealing with a list
  for (i++; i < exp.length; i++) {
    switch (state) {
      case "in_between":
        switch (exp[i]) {
          case "(":
            start = i;
            state = "in_list";
            break;
          case ")":
            return array_to_return;
          case " ":
            break;
          default:
            start = i;
            state = "in_token";
        }
        break;
      case "in_token":
        switch (exp[i]) {
          case ")":
            array_to_return.push(exp.slice(start, i));
            return array_to_return;
          case " ":
            array_to_return.push(exp.slice(start, i));
            state = "in_between";
            break;
          case "(":
            array_to_return.push(exp.slice(start, i));
            start = i;
            state = "in_list";
            break;
        }
        break;
      case "in_list":
        switch (exp[i]) {
          case "(":
            level++;
            break;
          case ")":
            if (level === 0) {
              array_to_return.push(expressionToTree(exp.slice(start, i + 1)));
              state = "in_between";
            } else if (level > 0) {
              level--;
            } else {
              throw "misformed list";
            }
            break;
        }
        break;
      default:
        throw "in a weird state";
    }
  }
  throw "misformed list";
}
