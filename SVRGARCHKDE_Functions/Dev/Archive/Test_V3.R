#library(rugarch)

value   <- coredata(res2$True)
q_fcast <- res2$FCast_Upper

VaRTest(alpha = 0.95, 
        value, 
        q_fcast, 
        conf.level = 0.95)


VaRTest(alpha = 0.05, 
        value, 
        res2$FCast_Lower, 
        conf.level = 0.95)

