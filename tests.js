/* global test, equal, deepEqual, throws */
/* global convertTreeObjIntoTreeArray, expressionToTree, qeval */
/* global extend_environment, define_variable, lookup_variable_value, empty_environment */
/* global eval_lambda */
/* global set_variable_value */
/* jshint globalstrict:true */
//
"use strict";

test("convertTreeObjIntoTreeArray", function() {
  deepEqual(convertTreeObjIntoTreeArray({
      array: []
    }), [],
    "convertTreeObjIntoTreeArray({array:[]}) returns []");
  deepEqual(convertTreeObjIntoTreeArray({
      array: ["hello", "meow"]
    }), ["hello", "meow"],
    "convertTreeObjIntoTreeArray({array:['hello', 'meow']}) returns ['hello', 'meow']");
  deepEqual(convertTreeObjIntoTreeArray({
      array: [{
        array: ["hello", "meow"],
      }, {
        array: ["woof", {
          array: []
        }, "chewy"]
      }]
    }), [
      ["hello", "meow"],
      ["woof", [], "chewy"]
    ],
    "convertTreeObjIntoTreeArray deals with multi nested arrays");
});

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

test("evaluating primitive types", function() {
  var env = empty_environment();
  equal(qeval("3", env), 3, "3 evaluates to 3", env);
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
  define_variable("a", "glass", env_1);
  equal(lookup_variable_value("a", env_3), "glass",
    "set_variable_value correctly overwrites old values");
});

test("evaluating primitive functions", function() {
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
  var lambda_exp = ["lambda", ["x"],
    ["*", "x", "3"],
    ["+", "1", "2"]
  ];
  var lambda_procedure = evaluate(lambda_exp, env);
  deepEqual(lambda_procedure.code, [
      ["*", "x", "3"],
      ["+", "1", "2"]
    ],
    "procedure code is an array of expressionTrees");
  qeval("(define meow (lambda (x) x))", env);
  equal(qeval("(meow 3)", env), 3,
    "defining meow as (lambda (x) x), evaluating (meow 3) results in 3");
  qeval("(define factorial (lambda (x) (if (= x 1) 1 (* x (factorial (- x 1))))))", env);
  equal(qeval("(factorial 10)", env), 3628800,
    "recursive functions work, (factorial 10) results in 3628800");
});

test("defining lambda shortcut", function() {
  var env = empty_environment();
  qeval("(define (meow x) (* x 2))", env);
  equal(qeval("(meow 3)", env), 6,
    "Using the shortcut (define (meow x) (* x 2)) and then evaluating (meow 3) results in 6");
});

test("overwriting definitions", function() {
  var env = empty_environment();
  qeval("(define meow 3)", env);
  qeval("(define meow 4)", env);
  equal(qeval("meow", env), 4,
    "Overwriting happens when a variable is defined with the same name as an old name");
  qeval("(define (cow x) (+ x 1))", env);
  qeval("(define (cow x) (+ x 2))", env);
  equal(qeval("(cow 2)", env), 4,
    "Overwriting happens when a procedure is defined with the same name as an old name");
});

test("anonymous procedures", function() {
  var env = empty_environment();
  equal(qeval("((lambda (x) (* x x)) 3)", env), 9,
    "Evaluating ((lambda (x) (* x x)) 3) results in 9");
});

test("more than one expression in a procedure", function() {
  var env = empty_environment();
  qeval("(define (meow x) (define cat 4) (* x cat))", env);
  equal(qeval("(meow 3)", env), 12,
    "Sequences of expressions in procedures are evaluated sequentially");
});

test("y operator", function() {
  var env = empty_environment();
  equal(qeval("((lambda (n) ((lambda (fact) (fact fact n)) (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1))))))) 4)", env), 24,
    "the y operator can be used to calculate factorials");
});
