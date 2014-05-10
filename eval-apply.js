/* exported evaluate */

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
function evaluate(expressionTree) {
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
      return expressionTree;
    }
  }

  // we know expressionTree is a list
  // but what kind of expression is it?
  switch (evaluate(expressionTree[0])) {
    case "if":
      return eval_if(expressionTree.slice(1));
  }
}

// for quick-evaluate
// always returns a string
function qeval(expression) {
  "use strict";
  return evaluate(expressionToTree(expression)).toString();
}

function eval_if(expressionTree) {
  "use strict";
  // expressionTree should look like this...
  // (3 1 4)
  if (evaluate(expressionTree[0])) {
    return evaluate(expressionTree[1]);
  } else if (expressionTree.length === 3) {
    return evaluate(expressionTree[2]);
  } else {
    return false;
  }
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
    } else {
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
