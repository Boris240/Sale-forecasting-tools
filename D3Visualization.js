// r2d3: nuage de points animé dans le temps
const width = 700, height = 500, margin = 50;

// Efface tout
div.selectAll("*").remove();

// SVG
const svg = div.append("svg")
  .attr("width", width)
  .attr("height", height);

// Simuler des données si r2d3.data est vide
if(!r2d3.data || r2d3.data.length === 0){
  const areas = ["A", "B", "C"];
  const years = d3.range(2000, 2010);
  r2d3.data = [];

  areas.forEach(area => {
    years.forEach(year => {
      r2d3.data.push({
        YEAR: year,
        mean: Math.random()*10 + (area.charCodeAt(0)-65)*2, // valeurs aléatoires
        Area: area
      });
    });
  });
}

// Couleur par Area
const color = d3.scaleOrdinal(d3.schemeSet2);

// Calcul min/max pour mapping brut
const years = r2d3.data.map(d => d.YEAR);
const sales = r2d3.data.map(d => d.mean);
const xMin = Math.min(...years), xMax = Math.max(...years);
const yMin = Math.min(...sales), yMax = Math.max(...sales);

// Titre
const title = svg.append("text")
  .attr("x", width/2)
  .attr("y", margin/2)
  .attr("text-anchor", "middle")
  .attr("font-size", "18px")
  .text("Nuage de points animé");

// Liste des années pour animation
const uniqueYears = Array.from(new Set(years)).sort((a,b)=>a-b);
let yearIndex = 0;

// Fonction d’animation
function animate(){
  if(yearIndex >= uniqueYears.length) yearIndex = 0;
  const currentYear = uniqueYears[yearIndex];

  // Filtrer les données pour l'année courante
  const frameData = r2d3.data.filter(d => d.YEAR === currentYear);

  // Data join
  const points = svg.selectAll("circle")
    .data(frameData, d => d.Area);

  // Enter + merge
  points.enter()
    .append("circle")
    .attr("r", 8)
    .attr("fill", d => color(d.Area))
    .attr("cx", d => margin + (d.YEAR - xMin)/(xMax - xMin)*(width - 2*margin))
    .attr("cy", d => height - margin - (d.mean - yMin)/(yMax - yMin)*(height - 2*margin))
    .append("title")
    .text(d => `${d.Area}: ${d.mean}`)
    .merge(points)
    .transition()
    .duration(r2d3.options.speed || 500)
    .ease(d3.easeCubicInOut)
    .attr("cx", d => margin + (d.YEAR - xMin)/(xMax - xMin)*(width - 2*margin))
    .attr("cy", d => height - margin - (d.mean - yMin)/(yMax - yMin)*(height - 2*margin));

  // Exit
  points.exit().remove();

  // Mettre à jour le titre
  title.text(`Année : ${currentYear}`);

  yearIndex++;
  setTimeout(animate, (r2d3.options.speed || 500) + 200);
}

// Lancer l’animation
animate();
