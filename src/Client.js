// module Client

exports.selectElement = function(el) {
  return function() {
    el.select();
  }
};
