module.exports = function(grunt) {
  "use strict";

  var sourceFiles = ["src/**/*.purs", "bower_components/**/src/**/*.purs"];

  grunt.initConfig({
    dotPsci: {
      src: sourceFiles
    },
    psc: {
      options: {
        main: "Multipac",
        modules: ["Multipac", "LevelMap", "Utils", "Types"]
      },
      all: {
        src: sourceFiles,
        dest: "dist/Main.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("default", ["psc:all", "dotPsci"]);
};
