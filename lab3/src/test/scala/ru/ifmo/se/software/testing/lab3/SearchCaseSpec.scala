package ru.ifmo.se.software.testing.lab3

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.junit.Assert._
import org.junit._
import org.openqa.selenium._
import org.openqa.selenium.chrome.ChromeDriver

class SearchCaseSpec {
  private implicit def unitToIO(action: => Unit): IO[Unit] = IO(action)

  private val searchUrl: String = "https://www.ivi.ru/search/"
  private val driverRef = Ref[IO].of[WebDriver](null).unsafeRunSync()
  private val pageCaseRef = Ref[IO].of[SearchCase[IO]](null).unsafeRunSync()

  private val searchText = "Семнадцать мгновений весны"
  private val searchTextTranslit = "Semnadcati Mngnoveniy"
  private val searchTextTranslate = "Seventeen Moments of Spring"
  private val searchTextEngMistyped = "Ctvyflwfnm vuyjdtybq dtcys"
  private val notExistingTitle = "AABBCCAABBCC"
  private val tabCharacters = "\t\t\t"
  private val specialCharacters = "\u0015\u0016\u0017"
  private val repeatedTitle = Iterator.fill(3)(searchText).toList.mkString(" ")
  private val veryLongTitle = Iterator.fill(30)(searchText).toList.mkString(" ")

  private val targetUrl = "https://www.ivi.ru/watch/17_mgnoveniy_vesny"
  private val mainPresets: Array[AnyRef] = List(
    "a" -> "Премьеры фильмов",
    "a" -> "Новинки подписки",
    "a" -> "Сериалы Amediateka",
    "a" -> "Высокий рейтинг"
  ).toArray
  private val thirdPresetUrl = "https://www.ivi.ru/search/?q=%D0%A1%D0%B5%D1%80%D0%B8%D0%B0%D0%BB%D1%8B%20Amediateka"

  @Before
  def setUp(): Unit = (for {
    _ <- unitToIO { System.setProperty("webdriver.chrome.driver", "src/test/resources/chromedriver")}
    driver <- driverRef.updateAndGet(_ => new ChromeDriver)
    _ <- pageCaseRef.updateAndGet(_ => new SearchCase[IO](searchUrl, driver))
  } yield ()).unsafeRunSync()

  @After
  def tearDown(): Unit =
    driverRef.get.map(_.quit()).unsafeRunSync()

  @Test
  def basicSearch(): Unit = (
      for {
        page <- pageCaseRef.get
        _ <- page.basicSearchProcedure(searchText)
      } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def basicTruncatedSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.basicSearchProcedure(searchText.dropRight(6))
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def basicTranslitSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.basicSearchProcedure(searchTextTranslit)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def basicMistypedSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.basicSearchProcedure(searchTextEngMistyped)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def basicRepeatedSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.basicSearchProcedure(repeatedTitle)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def basicVeryLongSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.basicSearchProcedure(veryLongTitle)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def translatedSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.simpleSearchProcedure(searchTextTranslate)
    } yield assertEquals("Should've not found anything", "Ничего не нашлось", page.emptyResultNotion.getText)
    ).unsafeRunSync()

  @Test
  def notExistingSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.simpleSearchProcedure(notExistingTitle)
    } yield assertEquals("Should've not found anything", "Ничего не нашлось", page.emptyResultNotion.getText)
    ).unsafeRunSync()

  @Test
  def emptyLineSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.simpleSearchProcedure("")
    } yield assertEquals("Should've stayed on the same page", searchUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def tabsSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.simpleSearchProcedure(tabCharacters)
    } yield assertEquals("Should've moved by the hints", thirdPresetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def specialCharacterSearch(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.simpleSearchProcedure(specialCharacters)
    } yield assertEquals("Should've stayed on the same page", searchUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def searchWithHints(): Unit = (
    for {
      page <- pageCaseRef.get
      _ <- page.hintsSearchProcedure(1, searchText)
    } yield assertEquals("Should've found correct page", targetUrl, page.driver.getCurrentUrl)
    ).unsafeRunSync()

  @Test
  def presetSearches(): Unit = (
    for{
      page <- pageCaseRef.get
      presets <- page.presetsRetrieveProcedure()
    } yield assertArrayEquals("Should contain main presets", mainPresets, presets.asInstanceOf[Array[AnyRef]])
  ).unsafeRunSync()

}
