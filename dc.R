library(readr)
library(dplyr)
library(zoo)
library(stringr)
library(tidyr)
library(purrr)
library(data.table)
library(reticulate)


script1_path <- "get_diocese_links.py"  
script2_path <- "scrape_diocese.py"               

py_run_file(script1_path)

# Verify the output file exists
output_file1 <- "diocese_links_with_names.csv"
if (file.exists(output_file1)) {
  cat("Output file", output_file1, "created successfully.\n")
} else {
  stop("Error: ", output_file1, " was not created. Check get_diocese_links_with_names.py for errors.")
}

# Step 2: Run scrape_diocese.py, which uses diocese_links_with_names.csv
cat("Running scrape_diocese.py...\n")
py_run_file(script2_path)
cat("Finished running scrape_diocese.py. Checking for output...\n")

# Verify the output file exists
output_file2 <- "stats_with_bishop.csv"
if (file.exists(output_file2)) {
  cat("Output file", output_file2, "created successfully.\n")
} else {
  stop("Error: ", output_file2, " was not created. Check scrape_diocese.py for errors.")
}

# Load and preview the final CSV
data <- read.csv(output_file2, encoding = "UTF-8")
cat("Preview of", output_file2, ":\n")
print(head(data))

# Optional: Preview the intermediate CSV
cat("Preview of", output_file1, ":\n")
intermediate_data <- read.csv(output_file1, encoding = "UTF-8")
print(head(intermediate_data))







df <- read.csv("stats_with_bishop.csv")

# 第一步：选择需要的变量
data <- df %>% 
  select(url, diocese, Country, Mailing.Address, Rite, Official.Web.Site, Telephone, Year, Catholics, 
         Total.Population, Percent.Catholic, Diocesan.Priests, Religious.Priests, 
         Total.Priests, Catholics.Per.Priest, Permanent.Deacons, Male.Religious, 
         Female.Religious, Parishes)

# 第二步：删除Year不为数字的行
data_filtered <- data %>%
  filter(!is.na(as.numeric(gsub(",", "", gsub("^\\s*$", "", as.character(Year))))))

# 第三步：转换数据类型（处理逗号分隔符和百分号）
data_cleaned <- data_filtered %>%
  mutate(
    Year = as.numeric(gsub(",", "", gsub("^\\s*$", "", as.character(Year)))),
    Catholics = as.numeric(gsub(",", "", as.character(Catholics))),
    Total.Population = as.numeric(gsub(",", "", as.character(Total.Population))),
    Percent.Catholic = as.numeric(gsub("[%,]", "", as.character(Percent.Catholic))),
    Diocesan.Priests = as.numeric(gsub(",", "", as.character(Diocesan.Priests))),
    Religious.Priests = as.numeric(gsub(",", "", as.character(Religious.Priests))),
    Total.Priests = as.numeric(gsub(",", "", as.character(Total.Priests))),
    Catholics.Per.Priest = as.numeric(gsub(",", "", as.character(Catholics.Per.Priest))),
    Permanent.Deacons = as.numeric(gsub(",", "", as.character(Permanent.Deacons))),
    Male.Religious = as.numeric(gsub(",", "", as.character(Male.Religious))),
    Female.Religious = as.numeric(gsub(",", "", as.character(Female.Religious))),
    Parishes = as.numeric(gsub(",", "", as.character(Parishes)))
  )

# 第四步：为每个教区确定合适的目标年份并创建Year_process
data_expanded <- data_cleaned %>%
  group_by(url) %>%
  mutate(
    earliest_year = {
      min_year <- min(Year, na.rm = TRUE)
      if (min_year >= 2010) min_year else 10 * round(min_year / 10)
    },
    Year_process = if_else(
      Year >= 2010,
      Year,
      10 * round(Year / 10)
    )
  ) %>%
  ungroup()

# 提取general_info并去除重复
general_info <- data_expanded %>% 
  select(url, diocese, Country, Mailing.Address, Rite, Official.Web.Site, Telephone) %>% 
  distinct()

# 检查并处理重复diocese
duplicate_dioceses <- general_info %>%
  group_by(diocese) %>%
  summarise(unique_urls = n_distinct(url), count = n()) %>%
  filter(count > 1 & unique_urls > 1)

duplicate_rows <- general_info %>%
  filter(diocese %in% duplicate_dioceses$diocese) %>%
  arrange(diocese)

# 定义替换名称（扩展到58行，包含Barcelona和France, Military）
new_names <- c(
  "Alep [Beroea, Halab]", "Alep [Beroea, Halab] (Armenian)", "Alep [Beroea, Halab] (Chaldean)",
  "Alep [Beroea, Halab] (Maronite)", "Alep [Beroea, Halab] (Melkite Greek)", "Alep [Beroea, Halab] (Syrian)",
  "Argentina, Faithful of the Eastern Rites", "Military Ordinariate of Argentina, Military",
  "Austria, Faithful of Eastern Rites", "Austria, Military",
  "Brazil, Faithful of the Eastern Rites", "Brazil, Military",
  "France, Faithful of Eastern Rites", "France, Military",
  "Italy, Faithful of the Ukrainian Catholic Church (Ukrainian)", "Italy, Military",
  "Poland, Faithful of Eastern Rites", "Poland, Military",
  "Spain, Faithful of Eastern Rites", "Spain, Military",
  "Archdiocese of Baghdad (Chaldean)", "Patriarchate of Baghdad (Chaldean)",
  "Barcelona (Spain)", "Barcelona (Venezuela)",
  rep("France, Military", 36) # Fill remaining 36 rows with "France, Military"
)

# 创建映射表，动态分配新名称
duplicate_map <- duplicate_rows %>%
  mutate(
    diocese_new = new_names[row_number()] # Assign new names based on row order
  ) %>%
  select(url, diocese_new)

# 更新general_info中的diocese名称
general_information_updated <- general_info %>%
  left_join(duplicate_map, by = "url") %>%
  mutate(diocese = coalesce(diocese_new, diocese)) %>% 
  select(-diocese_new)

# 填补Country空缺
general_information_updated <- general_information_updated %>%
  mutate(
    Country = case_when(
      is.na(Country) ~ "England",
      Country == "England" & row_number() == 1 ~ "Italy",
      TRUE ~ Country
    )
  )

# 处理时间序列数据：汇总并过滤Year_process >= 1950
data <- data_expanded %>%
  group_by(url, Year_process) %>%
  summarise(across(c(Catholics:Parishes), mean, na.rm = TRUE), .groups = "drop") %>%
  filter(Year_process >= 1950)

# Function to generate year sequence
generate_year_sequence <- function(min_year) {
  if (min_year <= 2010 & min_year >= 1950) {
    c(seq(from = min_year, to = 2010, by = 10), 2011:2025)
  } else {
    min_year:2025
  }
}

# 扩展年份序列
urls <- unique(data$url)
all_results <- tibble()
for (u in urls) {
  group_data <- data %>% filter(url == u)
  min_year <- min(group_data$Year_process, na.rm = TRUE)
  new_years <- generate_year_sequence(min_year)
  new_rows <- tibble(Year_process = new_years, url = u)
  result_df <- new_rows %>%
    full_join(group_data, by = c("url", "Year_process")) %>%
    arrange(Year_process)
  all_results <- bind_rows(all_results, result_df)
}

# 插值和填充缺失值
variables <- setdiff(names(all_results), c("url", "Year_process"))

# 创建_imputed flags
for (var in variables) {
  all_results[[paste0(var, "_imputed")]] <- ifelse(is.na(all_results[[var]]), NA, 0)
}

# 线性插值
all_results <- all_results %>%
  group_by(url) %>%
  mutate(across(all_of(variables), ~ na.approx(.x, x = Year_process, na.rm = FALSE, rule = 2))) %>%
  ungroup()

# 最近值填充（整体 + 2010年后限制）
all_results <- all_results %>%
  group_by(url) %>%
  mutate(across(all_of(variables), ~ na.locf(na.locf(.x, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE))) %>%
  mutate(across(all_of(variables), ~ {
    result <- .x
    post_2010_mask <- Year_process >= 2010
    if (any(post_2010_mask)) {
      post_2010_data <- .x[post_2010_mask]
      filled_data <- na.locf(na.locf(post_2010_data, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)
      result[post_2010_mask] <- filled_data
    }
    result
  })) %>%
  ungroup()

# 更新_imputed flags
for (var in variables) {
  imputed_col <- paste0(var, "_imputed")
  all_results[[imputed_col]] <- ifelse(is.na(all_results[[imputed_col]]) & !is.na(all_results[[var]]), 1, all_results[[imputed_col]])
}

# 处理additional notes
index_yNA <- which(is.na(as.numeric(df$Year)))
index_year_changeName <- index_yNA + 1
Name <- df$Year[index_yNA]
url_changeName <- df$url[index_yNA]
from <- df$Year[index_year_changeName]

data_name <- as.data.frame(cbind(url_changeName, Name, from))
data_name$from <- as.numeric(data_name$from)
data_name <- na.omit(data_name)

url_freq_table <- as.data.frame(table(data_name$url_changeName))
colnames(url_freq_table) <- c("url_changeName", "CountName")
data_name <- merge(data_name, url_freq_table, by = "url_changeName", all.x = TRUE)
data_name <- data_name[data_name$CountName != 1, ]
colnames(data_name)[colnames(data_name) == "url_changeName"] <- "url"

setDT(data_name)
data_name[, .idx := .I]
data_name <- data_name[, .SD[which.max(.idx)], by = .(url, Name)][order(.idx)][, .idx := NULL][]

additional_notes <- data_name[, 1:3]

# 生成Additional.Notes
get_year <- function(x) {
  y <- readr::parse_number(as.character(x))
  y[!(y >= 1000 & y <= 2100)] <- NA
  as.integer(y)
}

df2 <- additional_notes %>%
  mutate(
    from_year = get_year(from),
    diocese = as.character(Name)
  )

notes_by_url <- df2 %>%
  group_by(url) %>%
  arrange(from_year, .by_group = TRUE) %>%
  summarise(
    current_start_year = if (all(is.na(from_year))) NA_integer_ else max(from_year, na.rm = TRUE),
    prior_names = list({
      cs <- if (all(is.na(from_year))) Inf else max(from_year, na.rm = TRUE)
      nm <- diocese[!is.na(from_year) & from_year < cs]
      unique(nm)
    }),
    .groups = "drop"
  ) %>%
  mutate(
    Additional.Notes = pmap_chr(
      list(current_start_year, prior_names),
      function(cs, pn) {
        if (is.na(cs) || length(pn) == 0) return(NA_character_)
        paste0("Before ", cs, ", the diocese was called ", paste(pn, collapse = " / "))
      }
    )
  ) %>%
  select(url, Additional.Notes)

# 合并到general_info并生成最终web_data
table_general_info <- unique(data_expanded[, 1:5])
web_data <- merge(table_general_info, all_results, by = "url", all.x = TRUE)
web_data <- merge(web_data, notes_by_url, by = "url", all.x = TRUE)

# 去除diocese中的冒号和引号
web_data$diocese <- gsub('^[\\s"“”]+|[\\s"“”]+$', "", web_data$diocese)

# 最终输出
write.csv(web_data, "web_data.csv", row.names = FALSE)