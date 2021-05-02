package ru.ifmo.se.software.testing.lab3

import cats.effect.Sync
import org.openqa.selenium._
import org.openqa.selenium.support.PageFactory

import java.time.Duration
import scala.jdk.CollectionConverters._
import scala.util.Try

class FilmsLandingCase[F[_]: Sync](landingUrl: String, val driver: WebDriver) {

  def tagSuggestionList: List[WebElement] =
    driver
      .findElement(By.xpath(s"/html/body/div[1]/div/section[2]/div/div/div[4]/div/div[2]/div/div/div/ul"))
      .findElements(By.cssSelector("li.suggestion__item"))
      .asScala.toList
      .filterNot(_.getAttribute("class").contains("filter"))
      .map(_.findElement(By.cssSelector("a")))

  def compilationSuggestionList: List[WebElement] = driver
      .findElement(By.xpath("//section[@data-test='miniPromoBlock']"))
      .findElements(By.cssSelector(".miniPromoBlockCustom.miniPromoBlockCustom_version_4"))
      .asScala.toList

  def genresSuggestionList: List[WebElement] =
    driver.findElement(By.xpath("//section[@data-test='genresBlock']"))
      .findElements(By.cssSelector(".gallery__nbl-tile.nbl-tile.nbl-tile_type_compact.nbl-tile_style_aratus.nbl-tile_hasAvatar_0.nbl-tile_hasIcon_1"))
      .asScala.toList

  def customFilterButton: WebElement =
    driver.findElement(By.xpath("/html/body/div[1]/div/section[2]/div/div/div[4]/div/div[2]/div/div/div/ul/li[1]/span/div"))

  def filtersList: List[WebElement] =
    driver.findElement(By.cssSelector(".catalog-filters__filters-list.catalog-filters__filters-list.catalog-filters__filters-list_active"))
      .findElements(By.cssSelector(".catalog-filters__nbl-checkbox.nbl-checkbox.nbl-checkbox_style_jimu.nbl-checkbox_mode_withLabel"))
      .asScala.toList

  def dropFiltersButton: WebElement =
    driver.findElement(By.xpath("/html/body/div[3]/div/div[2]/div/div/div/div/div[2]/div/div/div[2]/button[1]"))

  def applyFiltersButton: WebElement =
    driver.findElement(By.xpath("/html/body/div[3]/div/div[2]/div/div/div/div/div[2]/div/div/div[2]/button[2]"))

  def tagsFilteringProcedure(tagNumber: Int): F[String] = Sync[F].delay {
    val origPage = driver.getWindowHandle
    driver.switchTo().newWindow(WindowType.TAB).get(landingUrl)
    tagSuggestionList(tagNumber).click()
    val currentUrl = driver.getCurrentUrl
    driver.switchTo().window(origPage)
    currentUrl
  }

  def compilationsFilteringProcedure(compilationNumber: Int): F[String] = Sync[F].delay {
    val origPage = driver.getWindowHandle
    driver.switchTo().newWindow(WindowType.TAB).get(landingUrl)
    compilationSuggestionList(compilationNumber).click()
    Thread.sleep(100)
    val currentUrl = driver.getCurrentUrl
    driver.switchTo().window(origPage)
    currentUrl
  }

  def genresFilteringProcedure(genresNumber: Int): F[String] = Sync[F].delay {
    val origPage = driver.getWindowHandle
    driver.switchTo().newWindow(WindowType.TAB).get(landingUrl)
    genresSuggestionList(genresNumber).click()
    Thread.sleep(100)
    val currentUrl = driver.getCurrentUrl
    driver.switchTo().window(origPage)
    currentUrl
  }

  def customFilterProcedure(filtersToActivate: List[Int]): F[Unit] = Sync[F].delay {
    customFilterButton.click()
    dropFiltersButton.click()
    Thread.sleep(50)
    val filters = filtersList.zipWithIndex.filter(entry => filtersToActivate.contains(entry._2)).map(_._1)
    Thread.sleep(50)
    filters.foreach(_.findElement(By.className("nbl-checkbox__box")).click())
    applyFiltersButton.click()
  }

  driver.manage.window.maximize()
  driver.manage.timeouts.implicitlyWait(Duration.ofSeconds(10))
  driver.get(landingUrl)

  PageFactory.initElements(driver, this)
}


