template <int TextureCount> struct Vertex {
	float x, y, z, rhw;
	Pixel Color;
	float uv[TextureCount * 2];

	inline float& u(DoubleWord index) {
		return uv[index * 2];
	}
	inline float& v(DoubleWord index) {
		return uv[(index * 2) + 1];
	}
};

template <class VertexType> inline DoubleWord getVertexShader() {
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX0;
}

template <> inline DoubleWord getVertexShader< Vertex<1> >() { 
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX1;
}

template <> inline DoubleWord getVertexShader< Vertex<2> >() {
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX2;
}

template <> inline DoubleWord getVertexShader< Vertex<3> >() {
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX3;
}

template <> inline DoubleWord getVertexShader< Vertex<4> >() {
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX4;
}

template <> inline DoubleWord getVertexShader< Vertex<5> >() {
	return D3DFVF_XYZRHW | D3DFVF_DIFFUSE | D3DFVF_TEX5;
}
