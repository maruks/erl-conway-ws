#!/bin/bash


CLIENT=~/Projects/conways-client

cd $CLIENT

lein clean ; lein cljsbuild once min

cd -

cp $CLIENT/resources/public/js/compiled/conways_client.js ./priv/static/
