\name{sideChannelAttack-package}
\alias{sideChannelAttack-package}
\alias{sideChannelAttack}
\docType{package}
\title{Side channel attack}
\description{
Most embedded devices require security and privacy protection. Cryptography algorithms and protocols are designed with the aim to provide such security. However, cryptanalytics attacks based on physical measures (such as the device power consumption named trace) realized on secure devices can be used to challenge their actual security. These attacks called side channel attacks and consist in retrieving secret data by observing physical properties of the device. Note that each observed physical properties are described with a vector of real values and each component of this vector is seen as a variable.

Side channel attacks are probably among the most dangerous attacks in cryptanalysis. However, we observe that many scientific papers presenting new side channel attack do not give any implementation of their attacks neither any data would allowed to verify the claimed results. In other side, when a researcher need to realize an attack, he/she has to implement it. This package has many purposes: first, it gives to the community an R implementation of each known attack/countermeasure as well as data to test it, second it allows to implement a side channel attack quickly and easily.
}
\details{
This package is divided in 4 blocks.

The first block is the \code{simulator} that allows to simulate a cryptographic device without any knowledge of VHDL, FPGA, etc. On the base of a key and messages, it provides a physical measure such as power consumption. Its goal is to facilitate and accelerate the collect of data, to compare attacks with the same dataset and to implement countermeasures. Note that simulator can be categorized into several groups depending on their level of abstraction: at the gate level, at the instruction level and the function level, etc. And note that you can obtain traces through \code{powerC} given in this package.

The second block is the \code{filter}, a feature selection, intended to reduce the number of points per trace and therefore to accelerate the attack.

The third is the \code{model} (an attack) which returns the key used by the cryptographic device knowing a trace.

The last block is the \code{verify} function who estimates the quality of the attacks and the corresponding countermeasures. It can be based on the execution time of the attack, the amount of memory used, the number of power consumption traces required or only by the number of times the model predicts correctly the key.
}
\author{Liran Lerman & Gianluca Bontempi & Olivier Markowitch}

\references{
	P. C. Kocher & J. Jaffe & B. Jun, (1999), Differential Power Analysis: Leaking Secrets, In Proc. Crypto '99, Springer-Verlag, LNCS 1666, pages 388-397.

	P. C. Kocher, (1996), Timing attacks on implementations of Diffie-Hellman, RSA, DSS, and other systems, Neal Koblitz, Advances in Cryptology - CRYPTO'96, volume 1109 de Lecture Notes in Computer Science, pages 104-113. Springer-Verlag.

	K. Gandolfi & C. Mourtel & F. Olivier, (2001), Electromagnetic analysis: Concrete results, CHES 2001, C. K. Koc, D. Naccache, and C. Paar, Eds., vol. 2162 of LNCS, pp. 255-265, Springer-Verlag.

	S. Chari & J. R. Rao & P. Rohatgi, (2002), Template Attacks , in CHES, volume 2523 of LNCS, pages 13-28. Springer.

	L. Lerman & G. Bontempi & O. Markowitch, (2011) Side Channel Attack: an Approach based on Machine Learning. In the Proceedings of 2nd International Workshop on Constructive Side-Channel Analysis and Security Design, COSADE 2011. 
}
\examples{
#data collection
data(powerC)
traces = powerC[,-301]
traces = traces[,1:100]
key = powerC[,301]
newIndice = sample(1:256)
traces = traces[newIndice,]
key = key[newIndice]+1

#model checking 
attack=verify.ho(model=gaussian,filter=filter.PCA,Xlearn=traces[1:128,],Ylearn=key[1:128],Xval=traces[129:256,],Yval=key[129:256],nbreVarX=c(2:3))
plot(attack)
}
