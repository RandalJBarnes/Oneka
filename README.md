# Oneka
Oneka is a command-line program for characterizing stochastic well-head protection zones around public 
water supply wells.

The model is based upon steady flow in a single, homogeneous, isotropic, flat-lying, aquifer. The aquifer 
can be confined, unconfined, or mixed.

The physical characteristics of the aquifer used in this model include: the base elevation of the aquifer,
the porosity of the aquifer, the hydraulic conductivity of the aquifer, and the aquifer thickness.  
The hydraulic conductivity is treated as an uncertain value, which is characterized by a user specified 
probability distribution. The aquifer thickness is treated as an uncertain value, which is characterized 
by a user specified probability distribution. 

The model allows for the inclusion of the following features: uniform infiltration, uniform flow, a pumping 
well of interest, and a set of neighboring discharge-specified wells.

The strength of the uniform infiltration, as well as the magnitude and direction of the uniform flow, are
determined by minimizing the modelling errors at a set of specified piezometers.  In addition to the
uncertain hydraulic conductivity, the model treats piezometric head measurements as uncertain: each head
measurement is characterized by an expected value and a standard deviation. Thus, the fitted infiltration and
uniform flow are also uncertain.

The output from this model is a capture zone probability map for the pumping well of interest.  The capture
zone probability map is generated using a particle tracking methodology.  The stochastic nature of this
capture zone is caused by the uncertainty of the hydraulic conductivity, the infiltration, and the uniform
flow.