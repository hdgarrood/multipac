module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs"],

    psc: {
      options: {
        main: "Chapter2",
        modules: ["Chapter2"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  
  grunt.registerTask("default", ["psc:all"]);
};
