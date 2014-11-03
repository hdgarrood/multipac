module.exports = function(grunt) {
  "use strict";

  var commonFiles = [
    "src/Common/**/*.purs",
    "bower_components/**/src/**/*.purs",
    "vendor/**/src/**/*.purs"
  ];
  var serverFiles = commonFiles.concat("src/Server/*.purs");
  var clientFiles = commonFiles.concat("src/Client/*.purs");
  var allFiles = commonFiles.concat("src/Server/*.purs", "src/Client/*.purs");

  grunt.initConfig({
    dotPsci: {
      src: allFiles
    },

    psc: {
      server: {
        options: {
          main: "Server",
          modules: ["Server"]
        },
        src: serverFiles,
        dest: "bin/server.js",
      },
      client: {
        options: {
          main: "Client",
          modules: ["Client"]
        },
        src: clientFiles,
        dest: "js/client.js",
      }
    },

    execute: {
      server: {
        src: ["bin/server.js"]
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("make", ["psc:client", "psc:server", "dotPsci"]);
  grunt.registerTask("run", ["make", "execute:server"]);
  grunt.registerTask("default", ["make"]);
};
