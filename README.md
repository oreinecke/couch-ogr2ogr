Couch-og2ogr
============

A CouchDB plugin that receives a shape file blob and then employs
[ogr2ogr](http://www.gdal.org/ogr2ogr.html) to convert it into a nice GeoJSON
to be returned to you. ogr2ogr is a command line tool that ships with every
release of [Quantum GIS](http://qgis.org/en/site/).

Installation
------------

Compile the single erl script and place it in a meaningful sub-folder of the
lib folder of your CouchDB installation, and then append the content of
[ogr2ogr.ini](priv/default.d/ogr2ogr.ini) to your local.ini. While these sound
like horrible instructions, they are probably the easiest way to install
CouchDB plugins on Windows.

On Linux you can do `make plugin`, upload the plugin slug to your CouchDB, and
post the appropriate plugin-data to `/_plugins`. This way it gets listed as a
regular plugin and can be uninstalled just as easy.

Configuration
-------------

Some config vars are set expicitely to `undefined`. This way they still show up
in the futon config page. The plugin won't try to do anything until they seem
to be valid. They are:
- `command`: what erlang would have to type to launch ogr2ogr. On Linux it is
  presumably just that, but for instance I have set it to `D:\Program
  Files\QGIS Dufur\bin\ogr2ogr.exe`.
- `GDAL_DATA`: provides the environment variable of the same name if it isn't
  part of the environment yet. In my case it points to `D:\Program Files\QGIS
  Dufur\share\gdal`, and is required by ogr2ogr to look up known projection
  definitions.
- `fallback_crs`: if ogr2ogr can't figure out the known projection name from an
  ill-formatted ESRI .prj-file, the plugin will tell it to transform it to that
  projection, e.g. EPSG:31497.
- `roles`: JSON array of user roles who are allowed to use the plugin. Admins
  have always permission and an empty array permits everyone.

HTTP Requests
-------------

###`GET /_ogr2ogr`

runs a few tests to see if everything was set up correctly, and returns its
best guess of what part of the configuration might be wrong.

###`POST /_ogr2ogr -HContent-type:application/json`

expects a JSON object like this in the request body:

``` json
{
  "shx": "AAAnCgAAAAAAAAAAAAAAAAAAAAAAAA etc. etc. AAAAAAAAAAAAAAAAADIAAAAK",
  "shp": "AAAnCgAAAAAAAAAAAAAAAAA etc. etc. KUVpslRBUsnDnUN8VUE=",
  "qpj": "UFJPSkNTWyJHZXJtYW55X1 etc. etc. FPUklUWVsiRVBTRyIsIjMxNDk1Il1dCg==",
  "prj": "UFJPSkNTWyJHZXJtYW55X1 etc. etc. 1d",
  "dbf": "A18HGgEAAABhAKEAAAAAAAA etc. etc. etc. AgICAgICAgICAgICAgICAgICAg"
}
```

where the field name denotes the file name extension and the value must be its
base64 encoded binary content. Any formats known to ogr2ogr should work in
principle, so I've made no restriction on the type or number of uploaded files.

It returns `HTTP/1.1 200 OK` and a GeoJSON FeatureCollection, or an error code
with a message that tells you what might have gone wrong.
