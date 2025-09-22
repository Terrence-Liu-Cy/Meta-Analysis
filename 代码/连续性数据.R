#R语言及其民间作者为我们提供了一系列程序包（R包/R packages）供我们调用
#在使用R包之前，我们需要将R包下载到我们的电脑中，通常我们使用install.packages("")命令即可自动下载R包
#也有一些R包没有经过官方登记，通常我们可以使用devtools包链接到github下载这些R包
#R包下载之后，使用的时候需要使用library命令调用
#不同R包之间调用的函数可能存在冲突，建议每次分析不要一次性调用太多作用相同的包
install.packages("meta")#下载meta包
install.packages("metafor")#下载metafor包
install.packages("robvis")#下载robvis包
install.packages("devtools")#下载devtools包，devtools包用于将R链接到github，下载github上的R包
library("devtools")#加载devtools包
devtools::install_github("MathiasHarrer/dmetar")#链接到github下载dmetar包
#加载R包，由于我们本次分析仅适用meta包就足以完成，所以我们只调用meta分析包
#dmetar、metafor包可以实现一些更高阶的功能，可以使用??dmetar、??metafor查看说明书
library(meta)
library(dmetar)
library(metafor)
getwd()#查看工作路径
setwd('C:\\Users\\cheems\\Desktop\\meta分析示例')#设置工作路径
#读取文件
metadata <- read.csv('contin.csv')#文件必须是csv格式，括号里面要和数据的文件名保持一致
#运行meta分析
#SM：选择效应量，可以为SM、SMD、OR等
#fixed = F, random =T：选择分析模型，F表示False（否定），T表述True（肯定）
metaresults <-metacont(n.e,mean.e,sd.e,n.c,mean.c,sd.c,
                       data=metadata,sm="MD",fixed = F, random =T, 
                       method.tau = "REML",hakn = TRUE,
                       studlab = paste(authors,year,sep="\t"))
summary(metaresults)#查看meta分析结果
#导出森林图，layout表示森林图样式，可以是meta、JAMA、REVMAN5，Prediction选择是否在森林图中显示预测区间
forest(metaresults, layout = "REVMAN5", prediction = T)
pdf("森林图.pdf",12,12)#导出森林图PDF
forest(metaresults, layout = "REVMAN5", prediction = T)
dev.off()
#运行亚组分析，subgroup用于设置分组依据，fixed = F, random =T用于选择分析模型
metasubgroup <- update(metaresults, fixed = F, random =T,subgroup = IF, tau.common = F)
summary(metasubgroup)#查看亚组分析结果
forest(metasubgroup, layout = "REVMAN5", prediction = T)
pdf("亚组分析森林图.pdf",12,12)#导出PDF
forest(metasubgroup, layout = "REVMAN5", prediction = T)
dev.off()
#进行meta回归
#~IF + Time + Base表示我设置的协变量是IF、Time和Base
regression<- metareg(metaresults, ~IF + Time + Base)
summary(regression)#查看meta回归结果
bubble(regression, studlab = T)
pdf("气泡图.pdf",12,12)#导出气泡图PDF
bubble(regression, studlab = T)# studlab用于设置是否在图中显示研究标签
dev.off()
#检验发表偏倚
funnel(metaresults, studlab = T)#漏斗图
pdf("漏斗图.pdf",12,12)
funnel(metaresults, studlab = T)#导出漏斗图PDF，studlab用于设置是否在图中显示研究标签
dev.off()
#Egger's检验#Begge检验
metabias(metaresults, method.bias = "linreg")#Egger's检验
metabias(metaresults, method.bias = "Begg")#Begge's检验
#绘制剪补法漏斗图
tf<-trimfill(metaresults)
funnel(tf,pch=ifelse(tf$trimfill,1,23),level=0.95,comb.random=T)
summary(tf)#查看剪补法结果
pdf("剪补法漏斗图.pdf")
funnel(tf)
dev.off()
#逐步剔除文献进行敏感性分析
c <- metainf(metaresults,pooled = "random")
summary(c)
pdf("敏感性分析.pdf",12,12)
forest(metainf(metaresults,pooled = "random"),comb.random=T, layout="REVMAN5")
dev.off()



#制作偏倚风险评估图
rm(list=ls())#清除前面运行的环境
library(robvis)#加载robvis包
getwd()
setwd('C:\\Users\\cheems\\Desktop\\meta分析示例')
data <- read.csv('bias.csv')#读取数据
#tool用于设置偏倚风险评估工具，可以是ROB1、ROB2、ROBINS-I、QUADAS-2
#weighted表示是否对偏倚风险加权
#想要设置ROB图的颜色的话，只需要加上一行：colour = c("#f442c8","#bef441","#000000")，里面的是十进制颜色代码
rob_summary(data = data,tool="ROB2",overall = TRUE,weighted = TRUE)
rob_traffic_light(data = data,tool = "ROB1",psize = 10)
pdf("overall.pdf",12,8)
rob_summary(data = data,tool="ROB2",overall = TRUE,weighted = TRUE)
dev.off()
pdf("traffic.pdf",10,40)
rob_traffic_light(data = data,tool = "ROB2",psize = 10)
dev.off()

