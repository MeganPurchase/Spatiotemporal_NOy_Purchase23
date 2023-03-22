using Statistics
using CSV
using DataFrames
using StatsBase

hours = 1:23
data_points_per_hour = 60
data = DataFrame(CSV.File("Summer/WR_W/WR_w_minutefluxes.csv"))
data
output = zeros(23)
fluxes_R = data[:,3]
for hour in hours
    start = (hour - 1) * data_points_per_hour + 1
    finish = hour * data_points_per_hour
    #output[hour] = mean(fluxes_R[start:finish])
    output[hour] = std(fluxes_R[start:finish])

end

output


df = DataFrame(wrg = output)

CSV.write("wrg_NOZNO2_N_SD.csv", df)