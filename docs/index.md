# Blog

## 01: Quick! Make a PCB!

A few weeks ago I brought a fantastic piece of electronic, the Pocket Operator
Arcade (PO-20) from Teenage Engineering. It's a software synthesizer on
small PCB with a grid of push buttons and LEDs, two potentiometers and a custom
[LCD
screen](https://macprovid.vo.llnwd.net/o43/hub/media/1001/12143/3338900.jpg)
like you would find on a Game & Watch.

<img
src="http://www.cheapmonday.com/Content/ProductContent/0402009001/0402009001_0_2.jpg"
width="200">

Here's a short video that demonstrates the features of the PO-20:
www.youtube.com/watch?v=W5PvXQq3DVQ

After playing a [little bit with
it](https://www.youtube.com/watch?v=CN5plqgpwa4), I had a feeling you may know
if you are reading this: I want to make my own! When I start a project like
this one, I jump right in and design a first PCB very quickly while I'm
still in the excitement of this brand new idea. The PCB is of course not very
good (two bodge wires this time) but now I can't go back :)

The first board was designed as daughter board for the STM32F4 DISCO. It has a
powerful microcontroller (Cortex-M4F ~160 Mhz) and an audio DAC with headphone
output, so the "only" thing left to do is the user interface
(buttons and LEDs).

![Wee Noise Make Discovery
 rev-A](../media/pictures/discovery/discovery_rev_A_1.JPG)

With this board I was able to start working on the software, play with
multiplexed LED and buttons, start a first version of the state machine and get
some sounds out.

But it's time for the second design. I'm a software engineer so
the electronic part of this project is the most challenging and interesting for
me.
