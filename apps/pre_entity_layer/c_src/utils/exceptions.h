/**
 * @doc Custom exception classes.
 *
 * @copyright 2012-2013 David H. Bronke and Christopher S. Case
 * Licensed under the MIT license; see the LICENSE file for details.
 */

#ifndef __EXCEPTIONS__
#define __EXCEPTIONS__


#include <exception>
#include <string>


template<typename T>
class BadIndex : public std::exception
{
public:
	BadIndex(T index);
	virtual const char* what() const throw();

private:
	T index;
}; // end BadIndex


// Implementation (can't be in a separate file; see http://stackoverflow.com/a/11641429)

template<typename T>
BadIndex<T>::BadIndex(T index) :
	index(index)
{
} // end constructor

template<typename T>
const char* BadIndex<T>::what() const throw()
{
	return "Bad index";
} // end what


#endif // __EXCEPTIONS__
