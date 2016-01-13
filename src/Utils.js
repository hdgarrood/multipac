// module Utils

exports.eachWithIndex_ = function(xs) {
  return function(f) {
    return function() {
      var l = xs.length;
      for (var i = 0; i < l; i++) {
        f(xs[i])(i)();
      }
    }
  }
};
