Hello Yixin, I'm sharing the files related to the analysis and my code. Note that the models by the name MPglm41.1, MPglm42.1 ang MPglm43.1 are the final ones used. I have used the glm function with quasipossion family. Moreover, because glm only takes on values between 0 and 1, I divided the prevalance of diseases by 100. I also used the argument weights = total population to account for the varying population in different counties. The dataset used for analysis is trimmed and stored in the form of excel file in the repository. 
