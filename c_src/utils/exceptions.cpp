#include <string>

#include "exceptions.h"


template<typename T>
BadIndex<T>::BadIndex(T index) :
	index(index)
{
} // end constructor

template<typename T>
const char* BadIndex<T>::what() const throw()
{
	std::string msg = "Bad index: " << index;
	return msg.c_str();
} // end what
