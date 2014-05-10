/* global test, equal, deepEqual, throws */
/* global expressionToTree, qeval */
/* jshint globalstrict:true */
//
"use strict";



test("expressionToTree", function() {
  throws(function() {
      // this has to be put in a function
      // so it doesn't execute right away
      expressionToTree("3(");
    },
    "throws an error when non-lists are malformed");
  deepEqual(expressionToTree("3"), "3",
    "non-lists are returned unchanged");
  deepEqual(expressionToTree("(1 2 3)"), ["1", "2", "3"],
    "input \"(1 2 3)\" should return [\"1\", \"2\", \"3\"]");
  deepEqual(expressionToTree("(lambda(x)x)"), ["lambda", ["x"], "x"],
    "input \"(lambda(x)x)\" should return [\"lambda\", [\"x\"], \"x\"]");
  deepEqual(expressionToTree("(  lambda(   x)   x   )"), ["lambda", ["x"], "x"],
    "input \"(  lambda(   x)   x   )\" should still return [\"lambda\", [\"x\"], \"x\"]. Spaces shouldn't matter");
  deepEqual(expressionToTree("   (lambda(x)x)"), ["lambda", ["x"], "x"],
    "input \"   (lambda(x)x)\" should return [\"lambda\", [\"x\"], \"x\"]. Spaces at the start shouldn't matter");
});

test("eval-if", function() {
  equal(qeval("(if true 1 0)"), 1,
    "(if true 1 0) evaluates to 1");
  equal(qeval("(if (if 3 0 1) 2 5)"), 5,
    "(if (if 3 0 1) 2 5) evaluates to 5");
});
