#!/bin/sh

# mkdir samples_dl
# cd samples_dl

# wget http://machines.hyperreal.org/manufacturers/Roland/TR-909/samples/TR909all.zip

# unzip TR909all.zip

# for file in *.WAV; do ffmpeg -i "$file" -ac 1 -f s16le -acodec pcm_s16le "$file.raw"; done

# mkdir /tmp/wnm-samples
# cp *.raw /tmp/wnm-samples/

IMG=wnm_fs.img
FSMAKER="fsmaker -f littlefs -i ${IMG}"
$FSMAKER init $((1 * 1024 * 1024))
$FSMAKER mkdir /samples
$FSMAKER import /samples/clap samples_dl/HANDCLP1.WAV.raw 
$FSMAKER import /samples/kick samples_dl/BT0A0DA.WAV.raw 
$FSMAKER import /samples/hh samples_dl/HHCD4.WAV.raw 
$FSMAKER import /samples/rim samples_dl/RIM63.WAV.raw 
$FSMAKER import /samples/snare  samples_dl/ST0T3S3.WAV.raw
