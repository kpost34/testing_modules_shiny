#**Testing Modules Shiny Apps**
This project contains versions of two mini-apps (t-test app, and Linear Model app) which represent
my first experience building Shiny apps using modules.


## Summary
The two scripts in this project (i.e., t_test_app.R, lm_app.R) contain multiple versions of each 
app. There's a basic non-modularized version of the app followed by a corresponding build using modules. 
Finally, there's a modularized version with added features. The objective of this project was
to begin creating simple Shiny apps using modules as means to developing more complex apps with
far greater organization and ability to update by modularizing their code.

Specifically, the t-test app allows a user to generate two samples from normal distributions by
specifying their 1) sample sizes, 2) population means, and 3) population standard deviations. The 
user can then view summary stats and boxplots of the samples as well as the results of a 2-sample
t-test. In the Linear Model App, the user can choose from all data frames in the datasets package
that contain at least two numeric variables. Next, the user selects two variables, which yields
three tabbed outputs: raw data and summary stats, a scatter plot with a toggle for a regression
line, and summary model information.

*Please note that this project contains only code as these apps have not been deployed.*


## Technologies
*R 4.3.0 ("2023-04-21" "Already Tomorrow")
  + shiny_1.7.4
  + tidyverse 2.0.0
  + DT 0.28
  + rstatix 0.7.2
  + ggiraph 0.8.7
  + datasets 4.3.0
  + broom 1.0.4
  + rlang 1.1.1
  + bslib 0.5.0
  
  
#### **Project Creator: Keith Post**
+ [Github Profile](https://github.com/kpost34) 
+ [LinkedIN Profile](https://www.linkedin.com/in/keith-post/)
+ [Email](mailto:keithhpost@gmail.com)


