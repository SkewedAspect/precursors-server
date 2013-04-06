#include <exception>


template<typename T>
class BadIndex : public std::exception
{
public:
	BadIndex(T index);
	virtual const char* what() const throw();

private:
	T index;
}; // end BadIndex
