/* global test, equal, deepEqual, throws */
/* global expressionToTree, qeval */
/* global extend_environment, define_variable, lookup_variable_value, empty_environment */
/* global set_variable_value */
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
  var env = empty_environment();
  equal(qeval("(if true 1 0)", env), 1,
    "(if true 1 0) evaluates to 1");
  equal(qeval("(if (if 3 0 1) 2 5)", env), 5,
    "(if (if 3 0 1) 2 5) evaluates to 5");
});

test("environment-stuff", function() {
  var env_1 = extend_environment(undefined);
  var env_2 = extend_environment(env_1);
  var env_3 = extend_environment(env_1);
  define_variable("a", 3, env_1);
  define_variable("c", 1, env_2);
  define_variable("a", 2, env_2);
  define_variable("b", "meow", env_3);
  define_variable("c", "hello", env_3);
  equal(lookup_variable_value("c", env_2), 1,
    "lookup_variable_value(...) finds the correct variable");
  equal(lookup_variable_value("a", env_2), 2,
    "lookup_variable_value(...) finds the correct variable again");
  equal(lookup_variable_value("a", env_3), 3,
    "lookup_variable_value(...) finds the correct variable a third time");
  throws(function() {
      set_variable_value("d", 5, env_3);
    },
    "set_variable_value throws error when variable isn't found");
  define_variable("a", "woof", env_1);
  equal(lookup_variable_value("a", env_3), "woof",
    "set_variable_value correctly sets variables");
});

test("primitives", function() {
  var env = empty_environment();
  equal(qeval("(* 3 6)", env), 18,
    "evaluating (* 3 6) results in 18");
  equal(qeval("(/ 6 2)", env), 3,
    "evaluating (/ 6 2) results in 3");
  equal(qeval("(+ 100 102.2)", env), 202.2,
    "evaluating (+ 100 102.2) results in 202.2");
  equal(qeval("(- 62.5 31.2)", env), 31.3,
    "evaluating (- 62.5 31.2) results in 31.3");
  equal(qeval("(= 34 34)", env), "true",
    "evaulating (= 34 34) results in true");
});

test("lambdas", function() {
  var env = empty_environment();
  qeval("(define meow (lambda (x) x))", env);
  equal(qeval("(meow 3)", env), 3,
    "defining meow as (lambda (x) x), evaluating (meow 3) results in 3");
  // qeval("(define factorial (lambda (x) (if 
});
