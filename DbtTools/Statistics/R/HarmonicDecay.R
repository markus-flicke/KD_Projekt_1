HarmonicDecay=function(n){
#  HD = HarmonicDecay(n)
# calculates the Harmonic Decay Numbers HarmonicDecay(i,n) for i=1...n
# HarmonicDecay(i,n) = sum(1/k) , k=i...n
# Laws:
# HarmonicDecay(1,n) = Harmonic number HN(n) = 1+1/2+...1/n
#                      this approximates to:  log(n)+ 0.5772156649+ 1/(2*n)
# HarmonicDecay(n,n) = 1/n
#
# INPUT
# n      n > 0 natural number
#
# OUTPUT
# HD(1:n)       HD(i) = HarmonicDecay(i,n)

# MT Sept 2016

if(n < 1) HD = NaN

OneToN= c(1:n) # 1...n 
HD=OneToN*NaN
OneOverN = 1/OneToN    # 1/1, ... 1/n
for(i in 1:n){
 HD[i] = sum(OneOverN[i:n])  # 1/i+ ...+ 1/n
} 
#plot(OneToN,HD,ylab = 'HarmonicDecay(i,n)',xlab = 'i',main = 'HarmonicDecay')
return(HD)
}
