matlabround = function(x) sign(x) * floor(abs(x)+0.5);
# This implements round half away from zero (or round half towards infinity)