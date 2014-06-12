/* exported evaluate, qeval, teval */
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
/* To evaluate, first we do a syntactic analysis
 *
 */
function evaluate(expressionTree, env) {
  "use strict";
  // analyze returns an analyzed procedure, which takes
  // an env as it's only argument
  return analyze(expressionTree)(env);
}

// returns a function that takes an environment
function analyze(expressionTree) {
  "use strict";
  if (typeof expressionTree === "number") {
    return function() {
      return expressionTree;
    };
  }

  if (typeof expressionTree === "string") {
    if (!isNaN(parseFloat(expressionTree))) {
      var number = parseFloat(expressionTree);
      return function() {
        return number;
      };
    } else if (expressionTree === "true") {
      return function() {
        return true;
      };
    } else if (expressionTree === "false") {
      return function() {
        return false;
      };
    } else {
      // !!! we should have an else if for string strings, and
      // we should also have it throw an error if a symbol isn't found
      return function(env) {
        var lookup = lookup_variable_value(expressionTree, env);
        if (typeof lookup === "undefined") {
          return expressionTree;
        } else {
          return lookup;
        }
      };
    }
  }

  // we know expressionTree is a list
  // but what kind of expression is it?
  switch (expressionTree[0]) {
    case "if":
      return analyze_if(expressionTree);
    case "cond":
      return analyze_cond(expressionTree);
    case "let":
      return analyze_let(expressionTree);
    case "define":
      return analyze_define(expressionTree);
    case "lambda":
      return analyze_lambda(expressionTree);

      // primitives
    case "*":
      return analyze_mult(expressionTree);
    case "/":
      return analyze_div(expressionTree);
    case "+":
      return analyze_add(expressionTree);
    case "-":
      return analyze_sub(expressionTree);
    case "=":
      return analyze_equals(expressionTree);
    default:
      // application
      return analyze_apply(expressionTree);
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

// timing a qeval
function teval(expression, env) {
  "use strict";
  console.time("timing");
  var thing_to_return = qeval(expression, env);
  console.timeEnd("timing");
  return thing_to_return;
}

function analyze_mult(expressionTree) {
  "use strict";
  // expressionTree looks like (* 3 4)
  var first = analyze(expressionTree[1]);
  var second = analyze(expressionTree[2]);
  return function(env) {
    return first(env) * second(env);
  };
}

function analyze_div(expressionTree) {
  "use strict";
  // expressionTree looks like (/ 3 4)
  var first = analyze(expressionTree[1]);
  var second = analyze(expressionTree[2]);
  return function(env) {
    return first(env) / second(env);
  };
}

function analyze_add(expressionTree) {
  "use strict";
  // expressionTree looks like (+ 3 4)
  var first = analyze(expressionTree[1]);
  var second = analyze(expressionTree[2]);
  return function(env) {
    return first(env) + second(env);
  };
}

function analyze_sub(expressionTree) {
  "use strict";
  // expressionTree looks like (- 3 4)
  var first = analyze(expressionTree[1]);
  var second = analyze(expressionTree[2]);
  return function(env) {
    return first(env) - second(env);
  };
}

function analyze_equals(expressionTree) {
  "use strict";
  // expressionTree looks like (= 3 4)
  var first = analyze(expressionTree[1]);
  var second = analyze(expressionTree[2]);
  return function(env) {
    return first(env) === second(env);
  };
}

function analyze_apply(expressionTree) {
  "use strict";
  var procedure, new_environment;
  var analyzed_arguments = [];

  // analyze the arguments
  for (var j = 1; j < expressionTree.length; j++) {
    analyzed_arguments.push(analyze(expressionTree[j]));
  }

  return function(env) {
    // if expressionTree looks like this: (meow 3 4 7)
    //
    // we gotta find the environment that the procedure points to
    if (typeof expressionTree[0] !== "object") {
      procedure = lookup_variable_value(expressionTree[0], env);
    } else {
      // we're looking at something like
      // ((lambda (x) (* 2 x)) 3)
      // this is an anonymous procedure
      procedure = evaluate(expressionTree[0], env);
      if (!is_procedure(procedure)) {
        throw "anonymous procedure is not a procedure!";
      }
    }

    // then extend that environment
    new_environment = extend_environment(procedure.env);
    // and populate it
    for (var i = 0; i < procedure.parameters.length; i++) {
      //                           should this be new_environment? ---V 
      define_variable(procedure.parameters[i], analyzed_arguments[i](env), new_environment);
    }
    return analyze_sequence(procedure.code)(new_environment);
  };
}

function analyze_sequence(sequence_of_expressionTrees) {
  "use strict";
  // first, analyze all the expressionTrees
  var analyzed_expressions = [];
  for (var i = 0; i < sequence_of_expressionTrees.length; i++) {
    analyzed_expressions.push(analyze(sequence_of_expressionTrees[i]));
  }

  function sequentially(exp1, exp2) {
    return function(env) {
      exp1(env);
      return exp2(env);
    };
  }

  function loop(first_exp, rest_of_exps) {
    if (rest_of_exps.length === 0) {
      return first_exp;
    }
    return loop(sequentially(first_exp, rest_of_exps[0]), rest_of_exps.slice(1));
  }
  var thing_to_return = loop(analyzed_expressions[0], analyzed_expressions.slice(1));
  return thing_to_return;
}

function analyze_if(expressionTree) {
  "use strict";
  var predicate = analyze(expressionTree[1]);
  var evaluate_on_true = analyze(expressionTree[2]);
  switch (expressionTree.length) {
    case 3: // expressionTree looks like this: (if 3 1)
      return function(env) {
        if (predicate(env)) {
          return evaluate_on_true(env);
        } else {
          return false;
        }
      };
    case 4: // expressionTree looks like this: (if 3 1 2)
      var evaluate_on_false = analyze(expressionTree[3]);
      return function(env) {
        if (predicate(env)) {
          return evaluate_on_true(env);
        } else {
          return evaluate_on_false(env);
        }
      };
    default:
      throw "if expressions must have a length of 3 or 4";
  }
}

function analyze_cond(expressionTree) {
  "use strict";
  var analyzed_conditions = [];
  var analyzed_executions = [];
  // analyze the conditions and executions
  for (var i = 1; i < expressionTree.length; i++) {
    var condition_to_analyze = expressionTree[i][0];
    if (condition_to_analyze === "else") {
      condition_to_analyze = "true";
    }
    analyzed_conditions.push(analyze(condition_to_analyze));

    var execution_to_analyze = expressionTree[i][1];
    analyzed_executions.push(analyze(execution_to_analyze));
  }

  return function(env) {
    for (var i = 0; i < analyzed_conditions.length; i++) {
      if (analyzed_conditions[i](env)) {
        return analyzed_executions[i](env);
      }
    }
  };
}

function analyze_let(expressionTree) {
  "use strict";
  // let's take our let expression and turn it into 
  // a call on an anonymous procedure
  // ie. (let ((a 3) (b 4)) (* a b))
  // becomes ((lambda (a b) (* a b)) 3 4)
  //
  var parameter_list = [];
  var argument_list = [];
  for (var i = 0; i < expressionTree[1].length; i++) {
    parameter_list.push(expressionTree[1][i][0]);
    argument_list.push(expressionTree[1][i][1]);
  }
  var code = expressionTree.slice(2);
  var lamb = ["lambda"];
  lamb.push(parameter_list);
  lamb = lamb.concat(code);

  var whole_expression = [lamb].concat(argument_list);
  return analyze(whole_expression);
}



function analyze_define(expressionTree) {
  "use strict";
  var value;
  // expressionTree should look like this...
  // (define variable_name variable_value)
  if (typeof expressionTree[1] !== "object") {
    // do we need to evaluate the variable name in a define?
    // the racket documentation says the variable name must
    // be an identifier, so I don't think so....
    //
    // make sure there's only one expression
    if (expressionTree.length !== 3) {
      throw "there should be a single expression after an identifier in a define";
    }
    value = analyze(expressionTree[2]);
    return function(env) {
      define_variable(expressionTree[1],
        value(env), env);
    };
  } else {
    // or this...
    // (define (function_name param1 param2) (* 2 x))
    // which we need to transform into this
    // (define function_name (lambda (param1 param2) (* 2 x)))

    // first, create the lambda
    var lamb = ["lambda"];
    lamb.push(expressionTree[1].slice(1));
    lamb = lamb.concat(expressionTree.slice(2));
    value = analyze(lamb);
    return function(env) {
      define_variable(expressionTree[1][0],
        value(env), env);
    };
  }
}

// a procedure should look like this. it should be an object
// procedure.parameters = [x, y, z, etc];
// procedure.code = an array of expressionTrees
// procedure.env points to the parent_frame
function analyze_lambda(expressionTree) {
  "use strict";
  // expression Tree should look like this...
  // (lambda (x y) (* x y))
  // or this
  // (lambda (x y) (expression 1) (expression 2) ... (expression n))
  var procedure = {};
  procedure.parameters = expressionTree[1];
  procedure.code = expressionTree.slice(2);
  return function(env) {
    procedure.env = env;
    return procedure;
  };
}

// x is a procedure if it is an object with
// parameters, code, and env members
function is_procedure(x) {
  "use strict";
  if (typeof x === "undefined" || typeof x.parameters === "undefined" ||
    typeof x.code === "undefined" || typeof x.env === "undefined") {
    return false;
  }
  return true;
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
    throw "variable can 't be found!";
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
 * NOTE: these return new arrays, they don't mess with originals
 */
