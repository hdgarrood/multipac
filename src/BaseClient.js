// module BaseClient

exports.startAnimationLoop = function(action) {
  var loop = {};

  var go = (function() {
    window.requestAnimationFrame(this.go);
    action();
  }).bind(loop);
  loop.go = go;

  var stop = (function() {
    this.go = function () { }
  }).bind(loop);
  loop.stop = stop;

  return function() {
    loop.go();
    return loop;
  };
};

exports.stopAnimationLoop = function(loop) {
  loop.stop();
};
