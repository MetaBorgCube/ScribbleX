Compiling
---------

Prerequisites: 
==============

Please ensure you have the following installed on your machine:
- [Idris 1.3.0](https://www.idris-lang.org/)
- [Spoofax 2.5.0](http://www.metaborg.org/en/latest/source/release/development.html)

Follow these steps to compile the protocol:
1. Generate the Idris interfaces for each of the actors
2. `make`

To generate the interfaces for each of the actors, follow the steps below:
- Open Spoofax
- Import the `lang.scribblex` language:
  1. In the command line: `git clone https://github.com/MetaBorgCube/ScribbleX`
  2. In Spoofax, click on the `File` menu, and choose `Import...`; unfold the `Maven` folder and choose `Existing Maven Projects`; click `Next >`; click the `Browse...` button and find the folder you cloned the ScribbleX repository into in step 1.
- In the Eclipse package explorer in Spoofax, unfold `lang.scribblex`, and go to the `examples/csb` folder; open the `csb.scrx` global protocol specification.
- Click on the `Spoofax` menu, navigate to the `Generate` submenu, and click `Idris`.

After following these steps, 6 files should now have appeared:
1. `csb_B.scrx`
2. `csb_B.idr`
3. `csb_C.scrx`
4. `csb_C.idr`
5. `csb_S.scrx`
6. `csb_S.idr`

Running `make` from the command line will compile the Idris protocols for each of the three actors in the `csb.scrx` specification.

Running
-------

After compiling (see above), three binaries should have appeared:
1. `bank`
2. `seller`
3. `customer`

Running each of these binaries in the order above from the command line will run the session typed communication protocol on your local machine, with interactive input/output via the command line.
