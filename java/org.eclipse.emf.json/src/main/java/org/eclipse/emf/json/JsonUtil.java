package org.eclipse.emf.json;

import java.io.IOException;
import java.util.Map;

import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonParseException;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.JsonToken;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.EcorePackage;

public class JsonUtil {

	static public EObject fromJson(String jsonTxt, EClass eClass) throws JsonParseException, IOException {
		EObject result = eClass.getEPackage().getEFactoryInstance().create(eClass);
		Map<String, EStructuralFeature> fieldFeatureMap = getFieldFeatureMap(eClass);
		
		JsonFactory f = new JsonFactory();
		JsonParser jp = f.createJsonParser(jsonTxt.getBytes());
		
		jp.nextToken();
		while (jp.nextToken() != JsonToken.END_OBJECT) {
			String fieldName = jp.getCurrentName();
			EStructuralFeature feature = fieldFeatureMap.get(fieldName);
			if (feature == null) {
				continue; // ignore unmapped feature
			}
			else if (feature instanceof EAttribute) {
				EDataType dataType = (EDataType) feature.getEType();
				if (feature.isMany()) {
					// process list of EDataType
				}
				else {
					if (dataType.getEPackage() == EcorePackage.eINSTANCE) {
						switch (dataType.getClassifierID()) {
						case EcorePackage.EBOOLEAN:
							result.eSet(feature, jp.getBooleanValue());
							break;
						case EcorePackage.EBYTE:
							result.eSet(feature, jp.getByteValue());
							break;
						case EcorePackage.ESHORT:
							result.eSet(feature, jp.getShortValue());
							break;
						case EcorePackage.EINT:
							result.eSet(feature, jp.getIntValue());
							break;
						case EcorePackage.ELONG:
							result.eSet(feature, jp.getLongValue());
							break;
						case EcorePackage.EFLOAT:
							result.eSet(feature, jp.getFloatValue());
							break;
						case EcorePackage.EDOUBLE:
							result.eSet(feature, jp.getDoubleValue());
							break;
						case EcorePackage.ESTRING:
						default:
							result.eSet(feature, jp.getText());
							break;
						}
					}
					else {
						result.eSet(feature, jp.getText());
					}
				}
			}
			
		}
		
		return result;
	}
	
	private static Map<String, EStructuralFeature> getFieldFeatureMap(EClass eClass) {
		throw new UnsupportedOperationException();
	}

	static public String toJson(EObject eObj) {
		throw new UnsupportedOperationException();
	}
	
}
