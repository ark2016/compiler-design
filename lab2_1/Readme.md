```bash
go build transform.go
```

```bash
.\transform.exe .\demo.go | Out-File -FilePath .\demo_transformed.go -Encoding UTF8
```

```bash
go run .\demo_transformed.go
```

```bash
./astprint demo_transformed.go
```
