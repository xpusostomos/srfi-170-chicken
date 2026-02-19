#!/bin/sh

chalk -o srfi-170.wiki srfi-170.scm &&
sudo chicken-doc-admin -E srfi-170.wiki srfi-170
