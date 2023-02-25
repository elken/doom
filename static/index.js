const getParentsUntil = (el, selector) => {
  let parents = [], _el = el.parentNode;
  while (_el && typeof _el.matches === 'function') {
    parents.unshift(_el);
    if (_el.matches(selector)) {
      return parents;
    } else {
      _el = _el.parentNode;
    }
  }
  return [];
};

function canSeeElement(element) {
  const rect = element.getBoundingClientRect();
  const top = rect.top;
  const bottom = rect.bottom;
  const height = window.innerHeight;

  return height - top > 0 && bottom > 0;
}

function setActive() {
  const sections = document.querySelectorAll("div[class^=org-src-container]");
  document.querySelectorAll("nav .active").forEach(activeElem => activeElem.classList.remove("active"));

  sections.forEach(current => {
    const sectionId = current.parentNode.getAttribute("id");
    const navId = sectionId.split("-").reverse()[0];
    const elem = document.querySelector("nav a[href*=" + navId + "]");

    if (navId.startsWith("org") && elem) {
      const parents = [...getParentsUntil(elem, 'div').slice(2), elem];

      if (canSeeElement(current)) {
        parents.forEach((activeElem) => activeElem.classList.add("active"));
        if (!canSeeElement(elem)) {
          elem.scrollIntoView({
            behavior: 'smooth',
            block: 'center',
            inline: 'center'
          });
        }
      }
    }
  });
}

window.onload = function () {
  document.addEventListener("scroll", setActive);

  setActive()
}
