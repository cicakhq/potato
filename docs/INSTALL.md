# Manual installation

## Dependencies

To do a standardlone installation, the following dependencies must be installed:

  - git
  - SBCL (compiled with SB-UNICODE support)
  - CouchDB (tested with 1.6.0)
  - RabbitMQ (tested with 3.4.3)
  - Java 8 SDK
  - Apache Solr (optional)
  - memcached
  - rabbitmq-c (C API for RabbitMQ, at least version 0.6.0)
  - libfixposix (https://github.com/sionescu/libfixposix)
  - libffi (https://sourceware.org/libffi/)
  - GCC and G++
  - Leiningen (Clojure build tool)
  - npm (for gulp. this dependency will be removed)

On a clean Ubuntu system, these dependencies are available from the following packages:

`git couchdb-server rabbitmq-server memcached solr-jetty libfixposix-dev libffi-dev gcc g++ nodejs nodejs-legacy`

### Initialise submodules

Potato depends on a number of submodules which are downloaded using the following command, which is run from the
the source directory:

```shell
git submodule init
git submodule update
```

### SBCL installation

SBCL is available as from the Ubuntu repositories, but is not compiled
with SB-UNICODE support. We recommend that an appropriate version is
downloaded from: http://www.sbcl.org/platform-table.html

### rabbitmq-c

Like SBCL, rabbitmq-c is also available from the Ubuntu repositories,
but currently is at version 0.5.4. This version is been problematic
with Potato, resulting in crashes when the application attempts to
close a RabbitMQ channel. It should not be used.

Currently, the most stable version seems to be 0.6.0 and it's the
recommended version.

## Install Apache Solr (optional)

Apache Solr is a text index engine that is used to provide the
realtime search in Potato. This is an optional component, and if it is
not installed, realtime search will not be available.

To install Solr, start by downloading the distribution from
https://lucene.apache.org/solr/ and extracting the file.

The Solr server can now be started by running the `bin/solr start`
command from the Solr application directory.

Copy the Solr configuration from the Potato distribution at
`deploy/roles/solr/files/potato` into the Solr application directory
at `server/solr`:

```
cp -r $POTATO_HOME/deploy/files/solr/potato $SOLR_HOME/server/solr
```

Finally, the potato configuration needs to be added to Solr. This is
done by opening a browser to the Solr server on port 8983. Then click
on "Core admin" followed by "Add core". In the dialog box that
appears, type the following:

  - name: `potato`
  - instanceDir: `potato`
  - dataDir: `data`
  - config: `solrconfig.xml`
  - schema: `schema.xml`

If everything worked correctly, the core should now be added.

## Quicklisp installation

Quicklisp is a dependency manager for Common Lisp, similar to
Leiningen for Clojure or gem for ruby. Potato depends on Quicklisp for
installation of dependent libraries.

First, download Quicklisp:

```
curl -O https://beta.quicklisp.org/quicklisp.lisp
```

Then, start SBCL and load the Quicklisp application:

```
sbcl --load quicklisp.lisp
```

At the SBCL prompt, run the following two commands:

```
(quicklisp-quickstart:install)
(ql:add-to-init-file)
```

Type Control-D or `(sb-ext:quit)` to exit the SBCL environment.

After Quicklisp has been installed you can remove the `quicklisp.lisp`
file.

## Compile the potato.bin application

From the root of the source tree, type the following command to build
the main application:

```
tools/build_binary.sh
```

If everything worked correctly, this should generate a binary in the
root directory named `potato.bin`. This application is the main entry
point to the application server.

If something went wrong during the compilation, the builder will print
an error message followed by a stack trace. This stack trace can be
somewhat overwhelming and contains lots of information that is
somewhat unnecessary. Usually the problems are caused by missing
dependencies and it should be fairly clear what needs to be done to
solve it.

## Install Leiningen

Leiningen is a project management system for Clojure. In Potato, this
is needed because the client side is written in Clojurescript where
Leiningen takes care of the compilation of Clojurescript code into
Javascript.

The installation instructions can be found at http://leiningen.org/
and is very simple. All that needs to be done is to download the
`lein` script and run it, which will download all the other
dependencies:

```
wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein
chmod +x lein
./lein
```

The rest of this document assumes that the `lein` script will be
placed somewhere in the `PATH`.

## Compile the Clojurescript code

Potato currently consists of two Clojurescript applications which are
built from the `web-app` directory:

```
cd web-app
lein with-profile -dev cljsbuild once prod admin-prod
```

## CSS compilation

### Install gulp

gulp is installed using npm. On Ubuntu this application is part of the
package `nodejs`.

The following command installs gulp globally:

```
sudo npm install -g gulp
```

### Install npm

Go to the `web-app` directory and run the following command:

```
npm install
```

### Run gulp

Run `gulp build` once to build all the CSS files.

# Potato server configuration

Edit the file `potato.cfg` (preferably after copying it to a different
location) and configure all parameters.

The default settings for the CouchDB, RabbitMQ and memcached should
work out of the box without any changes, but for security reasons it
may be advisable to preotect these services with passwords. However,
it is also assumed that these services are firewalled off from any
external users.

Currently only Amazon S3 is supported for file uploads, so an account
needs to be created and the appropriate configuration options filled
in in the S3-section of the configuration file. The parameter
`:s3-directory` can be any word, and can be used to separate multiple
Potato installations using the same bucket.

The parameter `:youtube-key` is a Youtube API key which is used when
accessing thumbnail images for messages that link to youtube videos.

### Imagemagick configuration

The application uses Imagemagick to perform image transformations, for
example when creating a thumbnail image when uploading an image to a
channel. This conversion is performed using the `convert` program that
is part of the Imagemagick package.

The configuration option `:imagemagick-convert-program` is used to
specify the path to the `convert` program. The default is
`/usr/bin/convert` which will work, but can leave the application
vulnerable if a user uploads an image that expands to a very large
size. To prevent this, it is recommended that a wrapper is used which
uses `ulimit` to limit the maximum memory used as well as the maximum
file size for the program. Here is an example of such wrapper:

```
#!/bin/sh

ulimit -m 200000
ulimit -v 200000
ulimit -f 400000

exec /usr/bin/convert "$@"
```

## Initialise the database

The database initialisation procedure performs two operations:

  - Rebuild all static CouchDB views
  - Flush the memcached cache

This procedure needs to be perfomed once before the system is started,
and after every update of the application where the CouchDB views have
been changed. It's recommended to run the initialisation procedure
every time the application has been updated.

Initialisation is run using the `--init` flag to the `potato.bin`
application:

```
$ ./potato.bin -c potato.cfg --init
Database was initialised successfully
```

## Starting the Potato server

The application consists of five separate services, which can be run
in different processes:

  - State server (manages the activity state of all users)
  - Index manager (monitors incoming messages and sends them to Solr for indexing)
  - Content processor (looks at the content of messages and updates
    them based on its content, for example by adding Youtube
    thumbnails to the message)
  - Email updates server (sends notification emails when a user have
    received notifications which has not been read)
  - Web server (the main web server)

Of these, all but the state server and index manager can be started on
multiple machines for load-blancing purposes.

In a small environment, all services can be started in the same process using the flag: `--full`.

When the system is started, the following text will appear:

```
$ ./potato.bin -c potato.cfg --full
 <INFO> [09:38:55] state-server state-server.lisp (start-state-server) - State server started
 <WARN> [09:38:55] potato-index potato-index.lisp (start-index-manager) - Solr server not available, index server not started
 <INFO> [09:38:55] potato.core chat-server.lisp (start-main) - Server started
 <INFO> [09:38:55] potato potato-main.lisp (start-message-processor-server) - Message processor started
```

In the example above, Solr was not available, so a warning message is displayed.

## Creating a default domain

At the highest level, Potato deals with a concept referred to as
"domains". One can think of a domain as a single "installation". A
normal self-hosted installation would only have a single domain since
all users are part of the same organisation.

The concept of multiple domains is implemented to support a single
Potato server serving multiple organisations, but the client
infrastructure to handle this is very rudimentary at best. To enable
it, set `(:allow-create-domain . t)` in the config file.

For this reason, a standard installation typically has a single
domain, which is configured such that any user that is registered is
automatically added to it. This can be set up using the following
command:

```
./potato.bin -c potato.cfg --cmd 'create-domain Foo true true'
```

This creates a domain called "Foo", which is public and has flag auto-join set.

For details about the "create-domain" command, type:

```
./potato.bin --cmd 'help create-domain'
```

The command responds with the ID of the new domain, which is a series
of hex digits (this is in fact the CouchDB key for the object in the
database).

## Creating user groups

The system supports user groups, where channels can be limited to a
given group. However, the infrastructure for managing this in an easy
wasy has not been implemented (mainly because the authors have not
actually needed this yet).

A command line interface for managing groups will be added.

## Creating channels

When creating a domain, a default channel is always created. There is
currently no way for a user to create new channels (other than private
conversations, which are implemented as channels behind the scenes).
The administrator can create channels using the `create-channel`
command. This command takes the domain ID and the name of the channel
as arguments. Once the channel is created, it will appear for all
users in the domain.
