echo off
echo Cleaning directory...
del c:\wamp\www\abap2cloud\*.php
echo Building %1
bundle exec ruby bin\abap2cloud compile examples\%1.abap > c:\wamp\www\abap2cloud\%1.php

