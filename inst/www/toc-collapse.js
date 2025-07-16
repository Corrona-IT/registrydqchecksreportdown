<script>
document.addEventListener("DOMContentLoaded", function () {
  const toc = document.querySelector(".toc");
  if (!toc) return;

  const headers = toc.querySelectorAll("li");

  headers.forEach(header => {
    const parentLevel = header.closest("ul").getAttribute("class") || "";
    const sublist = header.querySelector("ul");

    // Only add toggle to Heading 2 items (second-level TOC)
    if (sublist && parentLevel.includes("tocify")) {
      // Add toggle button
      const btn = document.createElement("button");
      btn.textContent = "+";
      btn.style.marginRight = "5px";
      btn.style.cursor = "pointer";

      btn.onclick = () => {
        const visible = sublist.style.display === "block";
        sublist.style.display = visible ? "none" : "block";
        btn.textContent = visible ? "+" : "−";
      };

      sublist.style.display = "none"; // Initially hide subheadings
      header.insertBefore(btn, header.firstChild);
    }
  });
});
</script>
