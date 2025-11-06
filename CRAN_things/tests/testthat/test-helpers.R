library(examly)

test_that("d_mode() modu doğru buluyor", {

  expect_equal( examly:::d_mode(c(1, 2, 2, 3, 3, 3, 4)) , 3 )
  expect_equal( examly:::d_mode(c(1, 1, 2, 2, 3)) , 1 )
  expect_true( is.na(examly:::d_mode(numeric(0))) )
  expect_true( is.na(examly:::d_mode(c(NA, NA, NA))) )
})

test_that("norm_letter() harfleri temizliyor ve doğruluyor", {

  expect_equal( examly:::norm_letter("a") , "A" )
  expect_equal( examly:::norm_letter("  b ") , "B" )
  expect_true( is.na(examly:::norm_letter("F")) )
  expect_equal( examly:::norm_letter(c("a", " G ", "c")) , c("A", NA, "C") )
})

test_that("parse_mc_bin() çoktan seçmeli puanlamasını doğru yapıyor", {

  expect_equal( examly:::parse_mc_bin(x = "A", key = "A"), 1 )

  expect_equal( examly:::parse_mc_bin(x = "B", key = "A"), 0 )

  expect_true( is.na(examly:::parse_mc_bin(x = NA, key = "A")) )

  expect_true( is.na(examly:::parse_mc_bin(x = "F", key = "A")) )

  expect_equal( examly:::parse_mc_bin(x = " a ", key = "A"), 1 )
  expect_equal( examly:::parse_mc_bin(x = "b", key = "A"), 0 )
})

test_that("parse_tf_bin() D/Y puanlamasını doğru yapıyor", {

  expect_equal( examly:::parse_tf_bin(x = "D", key = "D"), 1 )

  expect_equal( examly:::parse_tf_bin(x = "Y", key = "D"), 0 )

  expect_equal( examly:::parse_tf_bin(x = "DOGRU", key = "D"), 1 )
  expect_equal( examly:::parse_tf_bin(x = "YANLIŞ", key = "D"), 0 )
  expect_equal( examly:::parse_tf_bin(x = " False ", key = "D"), 0 )

  expect_true( is.na(examly:::parse_tf_bin(x = NA, key = "D")) )
  expect_true( is.na(examly:::parse_tf_bin(x = "Belki", key = "D")) )
})

test_that("parse_lc_bin() 1-0 kodlu puanlamayı yapıyor", {
  expect_equal( examly:::parse_lc_bin(1), 1 )
  expect_equal( examly:::parse_lc_bin(0), 0 )
  expect_equal( examly:::parse_lc_bin("1"), 1 )
  expect_equal( examly:::parse_lc_bin("0"), 0 )
  expect_true( is.na(examly:::parse_lc_bin(2)) )
  expect_true( is.na(examly:::parse_lc_bin(NA)) )
})

test_that("is_scored_01() 1-0 kodlu sütunları algılıyor", {
  expect_true( examly:::is_scored_01(c(1, 0, 1, 0, NA)) )
  expect_true( examly:::is_scored_01(c("1", "0", "1", NA)) )
  expect_false( examly:::is_scored_01(c(1, 0, 2, 0)) )
  expect_false( examly:::is_scored_01(c("A", "B", "C")) )
  expect_false( examly:::is_scored_01(numeric(0)) )
})

test_that("kr20() güvenirlik katsayısını hesaplıyor", {
  matris_test <- data.frame(
    m1 = c(1, 1, 0),
    m2 = c(1, 0, 0),
    m3 = c(1, 1, 1),
    m4 = c(0, 1, 0)
  )

  # p = [0.66, 0.33, 1.0, 0.33] -> p*q = [0.22, 0.22, 0, 0.22] -> sum(p*q) = 0.66
  # tot = [3, 3, 1] -> var(tot) = 1.333
  # k = 4
  # KR20 = (4/3) * (1 - 0.66 / 1.333) = 1.333 * (1 - 0.495) = 1.333 * 0.505 = 0.673

  expect_true( abs(examly:::kr20(matris_test) - 0.673) < 0.01 )

  expect_true( is.na(examly:::kr20(data.frame(m1 = c(1, 0)))) )
  expect_true( is.na(examly:::kr20(NULL)) )
  expect_true( is.na(examly:::kr20(data.frame(m1=c(1,1), m2=c(0,0)))) )
})

test_that("pbiserial_rest() stats::cor ile aynı sonucu veriyor", {
  set.seed(123)
  item_test <- c(1, 1, 1, 0, 0, 0, NA, 1)
  rest_skor <- c(10, 9, 8, 3, 2, 5, 10, 7)

  hesaplanan_r <- examly:::pbiserial_rest(item_test, rest_skor)

  beklenen_r <- stats::cor(item_test, rest_skor, use = "pairwise.complete.obs")

  expect_equal( hesaplanan_r, beklenen_r )

  expect_true( is.na(examly:::pbiserial_rest(c(NA, NA), c(1, 2))) )
  expect_true( is.na(examly:::pbiserial_rest(c(1, 1, 1), c(1, 2, 3))) )
})

test_that("student_counts() D-Y-B sayımlarını doğru yapıyor", {
  df_test <- data.frame(
    m1 = c(1, 1, 0, NA, 1),
    m2 = c(0, 1, 0, 1, NA)
  )

  sonuclar <- examly:::student_counts(df_test)

  beklenen_dogru <- c(1, 2, 0, 1, 1)
  beklenen_yanlis <- c(1, 0, 2, 0, 0)
  beklenen_bos <- c(0, 0, 0, 1, 1)

  expect_equal(sonuclar$Dogru, beklenen_dogru)
  expect_equal(sonuclar$Yanlis, beklenen_yanlis)
  expect_equal(sonuclar$Bos, beklenen_bos)

  bos_df_sonuc <- examly:::student_counts(data.frame())
  expect_equal(nrow(bos_df_sonuc), 0)
})

test_that("parse_lc_raw() uzun cevaplı puanları temizliyor", {
  veri <- c("10", "5.5", "0", "-2", "ikiyüz", NA, "1000001")
  beklenen <- c(10, 5.5, 0, NA, NA, NA, NA)
  expect_equal( examly:::parse_lc_raw(veri), beklenen )
})

test_that("norm_cols() sütun isimlerini temizliyor", {
  expect_equal( examly:::norm_cols(" m1, m2 ;m3"), c("m1", "m2", "m3") )

  expect_equal( examly:::norm_cols(c(" m1 ", "m2")), c("m1", "m2") )

  expect_equal( examly:::norm_cols(NULL), character(0) )
  expect_equal( examly:::norm_cols(""), character(0) )
})

test_that("difficulty_label_key() güçlük etiketlerini döndürüyor", {
  expect_equal( examly:::difficulty_label_key(0.3), "labels.difficulty.hard" )
  expect_equal( examly:::difficulty_label_key(0.7), "labels.difficulty.medium" )
  expect_equal( examly:::difficulty_label_key(0.9), "labels.difficulty.easy" )
  expect_true( is.na(examly:::difficulty_label_key(NA)) )
})

test_that("discrimination_decision_key() ayırıcılık etiketlerini döndürüyor", {
  expect_equal( examly:::discrimination_decision_key(0.1), "labels.discrimination.remove" )
  expect_equal( examly:::discrimination_decision_key(0.25), "labels.discrimination.consider_remove" )
  expect_equal( examly:::discrimination_decision_key(0.4), "labels.discrimination.keep" )
  expect_true( is.na(examly:::discrimination_decision_key(NA)) )
})

test_that("detect_id_cols() ID sütunlarını algılıyor", {
  cols <- c("Ad", "Soyad", "ogrenci no", "Madde1", "StudentID")
  expect_equal( examly:::detect_id_cols(cols), c("ogrenci no", "StudentID") )
  expect_equal( examly:::detect_id_cols(c("m1", "m2")), character(0) )
})
