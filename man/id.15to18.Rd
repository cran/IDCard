\name{id.15to18}
\alias{id.15to18}
\title{Update Chinese ID Card Number to Eighteen Digits}
\usage{
id.15to18(id.15)
}
\description{
The digits of the old version (before 2000 year) of 'Chinese ID Card Number' is 15, this package aims to update to the current version of 18 digits.
}
\arguments{
  \item{id.15}{numeric or character, the old version of Chinese ID Card Number}
}
\value{
  \item{id.18}{character, the new version of Chinese ID Card Number}
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

More advanced performance is coming!

}
\examples{
id.15to18(id.15="441900890101221")
}
