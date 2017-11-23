\name{is.id}
\alias{is.id}
\title{Checking the Chinses ID Card Number}
\usage{
is.id(id,code)
}
\description{
Checking the ID card number including length, area code, birth date, and last 3 number.
}
\arguments{
  \item{id}{vector of Chinese ID Card Number}
  \item{code}{please set as the sample}
}
\value{
  \item{id}{vector of Chinese ID Card Number, the same as the input}
  \item{length}{logic, whether the length of the 'id' is right or not, if 'FALSE', the following will be 'FALSE' either}
  \item{area}{logic, whether the area code of 'id' is right or not, if 'FALSE', the following will be 'FALSE' either}
  \item{birth}{logic, whether the birth date of 'id' is right or not, if 'FALSE', the following will be 'FALSE' either}
  \item{last3}{logic, whether the birth date of 'id' is right or not, if 'FALSE', the following will be 'FALSE' either}
}
\author{
Zhicheng Du<dgdzc@hotmail.com>, Yuantao Hao<haoyt@mail.sysu.edu.cn>
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Reference:

1. State Standard of the People's Republic of China: GB11643-1999
2. International Organization for Standardization: ISO7064:1983.MOD11-2

Update:

version 0.2.0: the input of id.15to18() can be a single element and a vector.

version 0.3.0: is.id() is added.

More advanced performance is coming!

}
\examples{
with(code,is.id(c("123456789123456","12345678912345X"),code))
}
