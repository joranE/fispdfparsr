library(fispdfparsr)
context("Cleaning results tables")

dst1 <- readRDS(file = "dst1.rds")
dst2 <- readRDS(file = "dst2.rds")

spr1 <- readRDS(file = "spr1.rds")
spr2 <- readRDS(file = "spr2.rds")

stg1 <- readRDS(file = "stg1.rds")
stg2 <- readRDS(file = "stg2.rds")

pts1 <- readRDS(file = "pts1.rds")

test_that("dst_clean() works on men's 15km", {
  expect_equal_to_reference(dst_clean(dst1,15),"dst1_ref.rds")
})

test_that("dst_clean_mass() works on women's 30km mass with bonus secs", {
  expect_equal_to_reference(dst_clean_mass(dst2,30),"dst2_ref.rds")
})

test_that("sprint_clean() works on sprint example 1", {
  expect_equal_to_reference(sprint_clean(spr1),"spr1_ref.rds")
})

test_that("sprint_clean() works on sprint example 2", {
  expect_equal_to_reference(sprint_clean(spr2),"spr2_ref.rds")
})

test_that("stage_clean() works on stage example 1", {
  expect_equal_to_reference(stage_clean(stg1),"stg1_ref.rds")
})

test_that("stage_clean() works on stage example 2", {
  expect_equal_to_reference(stage_clean(stg2),"stg2_ref.rds")
})

test_that("wc_pts_clean works on wc point example 1", {
  expect_equal_to_reference(wc_pts_clean(pts1),"pts1_ref.rds")
})
