use std::collections;
use std::env;
use std::fs;
use std::str;

#[derive(Debug)]
struct RecipeItem {
    resource: String,
    amount: u64,
}

impl str::FromStr for RecipeItem {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split(" ").collect();
        Ok(RecipeItem {
            resource: parts[1].to_string(),
            amount: parts[0].parse().expect("Invalid number of amount"),
        })
    }
}

#[derive(Debug)]
struct Recipe {
    inputs: Vec<RecipeItem>,
    output: RecipeItem,
}

impl str::FromStr for Recipe {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.splitn(2, " => ").collect();
        let inputs = parts[0]
            .split(", ")
            .map(|ri| ri.parse().expect("Invalid input"))
            .collect();
        let output = parts[1].parse().expect("Invalid output");
        Ok(Recipe {
            inputs,
            output,
        })
    }
}

fn read(file_name: &str) -> Vec<Recipe> {
    let contents: String = fs::read_to_string(file_name)
        .expect("Something went wrong reading the file");
    let recipes: Vec<Recipe> = contents
        .trim()
        .split("\n")
        .map(|line| line.parse().expect("Invalid Recipe"))
        .collect();
    return recipes;
}

#[derive(Debug)]
struct Stock<'a> {
    resource: &'a str,
    requested: u64,
    pending_requests: i64,
    produced: u64,
    recipe: Option<&'a Recipe>,
}

fn print_stock(stock: &collections::HashMap<&str, Stock>) {
    println!();
    for (k, v) in stock {
        println!("{:?} {:?}", k, v);
    }
    println!();
}

fn order(recipes: &Vec<Recipe>, target: &str, amount: u64, source: &str) -> u64 {
    let mut stock: collections::HashMap<&str, Stock> = collections::HashMap::new();

    for r in recipes {
        let out = stock.entry(&r.output.resource).or_insert(Stock {
            resource: &r.output.resource,
            requested: 0,
            pending_requests: 0,
            produced: 0,
            recipe: None,
        });
        out.recipe = Some(r);

        for i in &r.inputs {
            let inp = stock.entry(&i.resource).or_insert(Stock {
                resource: &i.resource,
                requested: 0,
                pending_requests: 0,
                produced: 0,
                recipe: None,
            });
            inp.pending_requests += 1;
        }
    }

    stock.get_mut(target).unwrap().requested = amount;

//    print_stock(&stock);

    let mut unprocessed = stock.len();
    while unprocessed > 0 {
        let mut ops: Vec<(&str, u64)> = Vec::new();

        for st in stock.values() {
            if st.pending_requests == 0 {
//                println!("{:?}", st.resource);

                if let Some(recipe) = st.recipe {
                    let req = ((st.requested as f64) / (recipe.output.amount as f64)).ceil() as u64;
//                    println!("Need {:?} recipes of {:?}", req, recipe);

                    for input in &recipe.inputs {
                        ops.push((&input.resource[..], input.amount * req));
                    }
                }
                ops.push((st.resource, 0));
                unprocessed -= 1;
            }
        }

        for (resource, req) in ops {
            let st = stock.get_mut(resource).unwrap();
            st.requested += req;
            st.pending_requests -= 1;
        }

//        print_stock(&stock);
    }

    stock[source].requested
}

fn order_max(recipes: &Vec<Recipe>, target: &str, source: &str, amount: u64) -> u64 {
    let mut min: u64 = 0;
    let mut max: u64 = 1;

    // estimate maximum
    while order(recipes, target, max, source) < amount {
        min = max;
        max *= 2;
    }

    // bisection to find estimate
    while min < max - 1 {
        let t = (min + max) / 2;
//        println!("Trying {}; diff = {}", t, max - min);
        let r = order(recipes, target, t, source);
        if r < amount {
            min = t;
        } else {
            max = t;
        }
    }

    // linear search to find exact
    let mut i = min;
    while order(recipes, target, i, source) < amount {
        i += 1;
    }

    i - 1
}

fn main() {
    let file_name: String = env::args().nth(1).expect("No argument given");
    let recipes = read(&file_name);

    let ores = order(&recipes, "FUEL", 1, "ORE");
    println!("Requested {:?} ores.", ores);

    let max = order_max(&recipes, "FUEL", "ORE", 1000000000000);
    println!("Max fuel {:?}.", max);
}
