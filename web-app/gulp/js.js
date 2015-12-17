/* Javascript related tasks -*- mode:js2 -*- */
var argv       = require('yargs').argv;
var gulp       = require('gulp');
var rev        = require('gulp-rev');
var gulpif     = require('gulp-if');
var notify     = require('gulp-notify');
var jshint     = require('gulp-jshint');
var stylish    = require('jshint-stylish');
var browserify = require('browserify');
var babelify   = require("babelify");
var debowerify = require('debowerify');
var collapse   = require('bundle-collapser/plugin');
var transform  = require('vinyl-transform');
var uglify     = require('gulp-uglify');
var tap        = require("gulp-tap");
var streamify  = require("gulp-streamify");
var plumber    = require("gulp-plumber");
var domain     = require("domain");
var gutil      = require('gulp-util');
var size       = require('gulp-size');
var buffer     = require('gulp-buffer');
var shim       = require('browserify-shim');
var extractor  = require('gulp-extract-sourcemap');

var isProd     = !!(argv.production);
var isDev      = !isProd;

var basePath   = global.basePath;

gulp.task('js:watch', function () {
    gulp.watch([ basePath.src+'js/**/*', '!.#*' ],  [ 'js:lint', 'js:compile' ]);
    gulp.watch(basePath.src+'ext/**/*', [ 'js:ext' ]);
});

gulp.task('js:lint', function () {
    gulp.src([basePath.src+'js/**/*', '!.#*'], { nodir: true })
    .pipe(jshint('.jshintrc'))
    .pipe(jshint.reporter(stylish));
});

// Inspired by https://gist.github.com/apfelbox/b2d5ee9fe32e5b42adb2
gulp.task('js:compile', function () {
    gulp.src([basePath.src+'js/*js', '!.#*'], { nodir: true })
    .pipe(plumber())
    .pipe(tap(
        function (file) {
            var d = domain.create();

            d.on("error", function (err) {
                gutil.log(
                    gutil.colors.red("Browserify compile error: "),
                    err.message,
                    "\n\t",
                    gutil.colors.cyan("in file"),
                    file.path
                );
                gutil.beep();
            });

            d.run(function () {
                file.contents =
                    browserify({
                        debug:   isDev,
                        noparse: [ 'jquery' ],
                        paths:   [ 'src/js', 'src' ]
                    })
                    .require(require.resolve(file.path), { entry: true })
                    .transform(babelify.configure({
                        ignore: /web-app\/libs/
                    }))
                    .transform(shim)
                    .transform(debowerify)
                    .plugin(collapse)
                    .bundle();

            });
        }
    ))
    .pipe(buffer())
    .pipe( extractor({
        removeSourcesContent:  true
    }) )
    .pipe(gulpif(isProd,streamify(uglify({
        outSourceMap:     true,
        preserveComments: 'some'
    }))))
    .pipe(buffer())
    .pipe(gulpif(
        isProd,size({
            title:     'Javascript production',
            gzip:      true,
            showFiles: false
        }), size({ // else clause
            title:     'Javascript development',
            showFiles: true
        })))
    .pipe(rev())
    .pipe(gulp.dest(basePath.dest+'js'))
    .pipe(notify({
        title:   "Potato's Gulp",
        message: "Javascript compiled for "+(isProd? "production":"development"),
        onLast:  true
    }))
    .pipe(rev.manifest('js.manifest'))
    .pipe(gulp.dest(basePath.tmpl));
});
