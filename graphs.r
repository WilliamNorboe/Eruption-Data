data = c(4.4,3.9,4,4,3.5,4.1,2.3,4.7,1.7,4.9,1.7,4.6,3.4,4.3,1.7,3.9,3.7,3.1,4,
1.8,4.1,1.8,3.2,1.9,4.6,2,4.5,3.9,4.3,2.3,3.8,1.9,4.6,1.8,4.7,1.8,4.6,1.9,3.5,4,
3.7,3.7,4.3,3.6,3.8,3.8,3.8,2.5,4.5,4.1,3.7,3.8,3.4,4,2.3,4.4,4.1,4.3,3.3,2,4.3,
2.9,4.6,1.9,3.6,3.7,3.7,1.8,4.6,3.5,4,3.7,1.7,4.6,1.7,4,1.8,4.4,1.9,4.6,2.9,3.5,
2,4.3,1.8,4.1,1.8,4.7,4.2,3.9,4.3,1.8,4.5,2,4.2,4.4,4.1,4.1,4,4.1,2.7,4.6,1.9,
4.5,2,4.8,4.1,4.1,4.2,4.5,1.9,4.7,2,4.7,2.5,4.3,4.4,4.4,4.3,4.6,2.1,4.8,4.1,4,4,
4.4,4.1,4.3,4,3.9,3.2,4.5,2.2,4.7,4.6,2.2,4.8,4.3,3.8,4,4.1,1.8,4.4,4,2.2,5.1,
1.9,5,4.4,4.5,3.8,4.3,4.4,2.2,4.8,1.9,4.7,1.8,4.8,2,4.4,2.5,4.3,4.4,1.9,4.7,4.3,
2.2,4.7,2.3,4.6,3.3,4.2,2.9,4.6,3.3,4.2,2.6,4.6,3.7,1.8,4.7,4.5,4.5,4.8,2,4.8,
1.9,4.7,2,5.1,4.3,4.8,3,2.1,4.6,4,2.2,5.1,2.9,4.3,2.1,4.7,4.5,1.7,4.2,4.3,1.7,
4.4,4.2,2.2,4.7,4,1.8,4.7,1.8,4.5,2.1,4.2,2.1,5.2,2)


# 1.
hist(data, xlab = "Bin", ylab = "Frequency Count", main = "Frequency Histogram of Eruption Duration")

# 2.
boxplot(data, main = "Boxplot of Eruption Time")

# 3.
quantiles = quantile(data, probs=c(0.95, 0.97, 0.99))
print(quantiles)

# 4.


# Red is short blue is long
dataColors = ifelse(data <= 3, "red", "blue")

plot(data[-length(data)], data[-1],
     main = "Scatter Plot of Eruption Durations",
     xlab = "Current Eruption Duration",
     ylab = "Next Eruption Duration",
     col = dataColors[-length(dataColors)])
legend("topright", legend = c("Short Eruption", "Long Eruption"), col = c("red", "blue"), pch = 19)

# 5
abline(h = 3, v = 3, col = "black")
#Observation: Majority of the points are long eruptions that are followed by long eruptions
# 6

longs = 0
shorts = 0
longLong= 0
longShort = 0
shortLong = 0
shortShort = 0
for(i in 2:length(data)-1){
  if(dataColors[i] == "blue"){
    longs = longs + 1
    if(dataColors[i+1] == "blue"){
      longLong = longLong + 1
    }
    else{
      longShort= longShort + 1
    }
  }
  else if(dataColors[i] == "red"){
    shorts = shorts + 1
    if(dataColors[i+1] == "blue"){
      shortLong = shortLong + 1
    }
    else{
      shortShort = shortShort + 1
    }
  }
}
longLong = longLong/length(data)
longShort = longShort/length(data)
shortLong = shortLong/length(data)
shortShort = shortShort/length(data)

cat("Long followed by Long: ", longLong, "\n")
cat("Long followed by Short: ", longShort, "\n")
cat("Short followed by Long: ", shortLong, "\n")
cat("Short followed by Short: ", shortShort, "\n")
