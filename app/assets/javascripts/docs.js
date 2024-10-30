(() => {
  "use strict";

  const frameworkTabs = document.querySelectorAll("#framework-tabs a");

  // Select current tab from URL
  const currentTab = document.querySelector(
    `#framework-tabs a[href="${window.location.hash}"]`
  );

  if (!!currentTab) {
    bootstrap.Tab.getOrCreateInstance(currentTab).show();
  }

  frameworkTabs.forEach((tab) => {
    tab.addEventListener("click", (event) => {
      event.preventDefault();
      window.history.pushState(null, null, tab.getAttribute("href"));
    });
  });

  // Hide offcanvas on mobile devices
  const sidebarNav = document.querySelector("#sidebar-nav");

  sidebarNav.addEventListener("shown.bs.offcanvas", (event) => {
    frameworkTabs.forEach((tab) => {
      tab.addEventListener("click", (event) => {
        event.preventDefault();
        bootstrap.Offcanvas.getInstance(sidebarNav).hide();
      });
    });
  });
})();
