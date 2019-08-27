run: clean generateWorld
	ghc -isrc src/Tracer.hs && echo "Running Tracer ..." && ./Tracer > out.ppm

generateWorld:
	python src/generate_world.py > src/ShapesWorld.hs

clean:
	rm -f *.o *.hi

