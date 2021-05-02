package ru.ifmo.se.software.testing.lab3

import cats.effect.Sync
import org.openqa.selenium._
import org.openqa.selenium.support.PageFactory

import java.time.Duration
import scala.jdk.CollectionConverters._

class SearchCase[F[_]: Sync](landingUrl: String, val driver: WebDriver) {

  def searchInput: WebElement =
    driver findElement By.xpath(s"/html/body/div[1]/div/div/div[2]/div/div/section[1]/div/div/form/div/div/input")

  def resultLink(number: Int): WebElement =
    driver findElement By.xpath(s"/html/body/div[1]/div/div/div[2]/div/div/div/section/div/div/div/div/div/ul/li[$number]/a")

  def hintLink(number: Int): WebElement =
    driver findElement By.xpath(s"/html/body/div[1]/div/div/div[2]/div/div/section[2]/div/div/div/div/div[$number]/a")

  def presetsButtons: Array[(String, String)] =
    driver
      .findElement(By.xpath("/html/body/div[1]/div/div/div[2]/div/div/section[2]/div/div/div/div"))
      .findElements(By.className("preset"))
      .asScala.toList
      .map(element =>
        element.findElement(By.cssSelector(".nbl-link.nbl-link_style_chaf"))
      )
      .map(element => element.getTagName -> element.getText)
      .toArray

  def basicSearchProcedure(searchText: String): F[Unit] = Sync[F].delay {
    searchInput.sendKeys(searchText + Keys.ENTER)
    val result = resultLink(1)
    result.click()
  }

  def hintsSearchProcedure(number: Int, searchText: String): F[Unit] = Sync[F].delay {
    searchInput.sendKeys(searchText)
    val hint = hintLink(number)
    hint.click()
  }

  def presetsRetrieveProcedure(): F[Array[(String, String)]] = Sync[F] delay presetsButtons

  driver.manage.window.maximize()
  driver.manage.timeouts.implicitlyWait(Duration.ofSeconds(10))
  driver.get(landingUrl)

  PageFactory.initElements(driver, this)
}


