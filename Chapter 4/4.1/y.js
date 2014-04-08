function factorial(number) {
  "use strict";
  return (function(run) {
    return run(run, number);
  })
  (function(recurse, num) {
    if (num === 1) {
      return 1;
    } else {
      return num * recurse(recurse, num - 1);
    }
  });
}
