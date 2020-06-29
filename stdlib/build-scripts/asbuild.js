import { promises as fs } from 'fs'
import process from 'process'
import { exec as _exec } from 'child_process'
import util from 'util'
import path from 'path'

const exec = util.promisify(_exec)
const dir = 'stdlib-external'

const base = path.join(process.cwd(), dir)
const options = { cwd: base }

const buildWasm = async source => {
  const target = source.slice(0, source.length - 2) + 'wasm'
  const { stdout, stderr } = await exec(
    `"../node_modules/.bin/asc" ${source} -o ${target} -O3 --runtime none --importMemory`,
    options
  )
  process.stdout.write(stdout)
  process.stderr.write(stderr)
}

(async() => {

  const files = (await fs.readdir(dir))
    .filter(f => f.endsWith('.ts'))
  
  await Promise.all(files.map(buildWasm))

})().catch(err => {
  console.error('failed to build stdlib-external')
  console.error(err.message)
  console.error(err.stack)
  process.exitCode = 1
})
