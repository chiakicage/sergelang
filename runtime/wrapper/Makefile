CPP_SRC       = $(sort $(wildcard *.cpp))           
OBJ		    = $(patsubst %.cpp,%.o,$(CPP_SRC))

all: $(OBJ)

%.o:%.cpp
	$(CXX) $(CXXFLAGS) -c $<

clean:
	$(shell rm *.o 2> /dev/null)