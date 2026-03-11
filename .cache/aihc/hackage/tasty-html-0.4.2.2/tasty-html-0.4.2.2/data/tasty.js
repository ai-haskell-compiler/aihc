(() => {
  const qs = document.querySelector.bind(document);
  const allGroups = [...document.getElementsByClassName("group")];

  const hasFailure = qs(".fail") !== null;
  let rolledUp = 0;

  const expand = qs("#expand-all");

  const updateExpandDisabled = () => {
    if (rolledUp === 0) {
      expand.setAttribute("disabled", "");
    } else {
      expand.removeAttribute("disabled");
    }
  };

  expand.addEventListener("click", () => {
    for (group of allGroups) {
      if (group.classList.contains("rollup")) {
        rolledUp--;
      }
      group.classList.remove("rollup");
    }
    updateExpandDisabled();
  });
  expand.classList.remove("hidden");

  allGroups.forEach(group => {
    if (hasFailure && !group.classList.contains("fail")) {
      group.classList.add("rollup");
      rolledUp++;
    }

    group.addEventListener("click", ev => {
      group.classList.toggle("rollup");
      rolledUp += group.classList.contains("rollup") ? 1 : -1;
      updateExpandDisabled();
    });
    // prevent annoying highlighting when toggling group
    group.addEventListener("mousedown", ev => {
      if (event.detail > 1) { event.preventDefault(); }
    }, false);
  });

  updateExpandDisabled();
})();
