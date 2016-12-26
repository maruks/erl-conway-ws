#!/bin/bash


CLIENT=~/Projects/conway-gui

cd $CLIENT

lein clean ; lein cljsbuild once min

cd -

cp $CLIENT/resources/public/js/compiled/conways_client.js ./priv/static/
