module.exports = function(grunt) {
  "use strict";

  var sourceFiles = [
    "src/**/*.purs",
    "bower_components/**/src/**/*.purs",
    "vendor/purescript-*/src/**/*.purs"
  ];

  grunt.initConfig({
    dotPsci: {
      src: sourceFiles
    },

    pscMake: {
      all: {
        src: sourceFiles
      }
    },

    copy: [
      {
        expand: true,
        cwd: "output",
        src: ["**"],
        dest: "tmp/node_modules/"
      },
      {
        src: ["js/client.js"],
        dest: "tmp/client.js"
      },
      {
        src: ["js/server.js"],
        dest: "tmp/server.js"
      }
    ],

    browserify: [
      {
        src: ["tmp/client.js"],
        dest: "static/js/game.js"
      }
    ]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-browserify");
  grunt.loadNpmTasks("grunt-contrib-copy");

  grunt.registerTask("make", ["pscMake", "copy", "dotPsci", "browserify"]);
  grunt.registerTask("run", ["make", "execute:server"]);
  grunt.registerTask("default", ["make"]);
};
